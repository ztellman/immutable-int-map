(ns
  ^{:doc "A straightforward port of Okasaki and Gill's \"Fast Mergeable Integer Maps`\",
          which can be found at http://ittc.ku.edu/~andygill/papers/IntMap98.pdf"}
  immutable-int-map
  (:require [primitive-math :as p]))

(definterface INode
  (merge [node f])
  (entries [acc])
  (assoc [^long k ^long epoch v])
  (dissoc [^long k ^long epoch])
  (update [^long k ^long epoch f])
  (get [^long k default]))

(set! *warn-on-reflection* false)

(definline ^:private off? [x m]
  `(p/== 0 (p/bit-and ~x ~m)))

(definline ^:private bit-mask [x m]
  `(p/bit-and (p/bit-or ~x (p/dec ~m)) (p/bit-not ~m)))

(definline ^:private match-prefix? [k p m]
  `(p/== (bit-mask ~p ~m) (bit-mask ~k ~m)))

(definline ^:private lowest-bit [x]
  `(p/bit-and ~x (p/bit-not (p/dec ~x))))

(defn- highest-bit ^long [^long x ^long m]
  (loop [x (p/bit-and x (p/bit-not (p/dec m)))]
    (let [m (lowest-bit x)]
      (if (p/== x m)
        m
        (recur (p/- x m))))))

(definline ^:private branching-bit [p0 m0 p1 m1]
  `(highest-bit (p/bit-xor ~p0 ~p1) (p/max 1 (p/* 2 (p/max ~m0 ~m1)))))

(declare branch leaf join)

(definline ^:private join [epoch p0 m0 t0 p1 m1 t1]
  `(let [m# (branching-bit ~p0 ~m0 ~p1 ~m1)]
     (if (off? ~p0 m#)
       (branch (bit-mask ~p0 m#) m# ~epoch ~t0 ~t1)
       (branch (bit-mask ~p0 m#) m# ~epoch ~t1 ~t0))))

(set! *warn-on-reflection* true)

(deftype Empty []
  INode
  (entries [_ acc]
    acc)
  (get [_ _ default]
    default)
  (dissoc [this _ _]
    this)
  (assoc [_ k epoch v]
    (leaf k epoch v)))

(deftype Branch
  [^long prefix
   ^long mask
   ^long epoch
   ^INode ^:volatile-mutable left
   ^INode ^:volatile-mutable right]
  INode
  (entries [_ acc]
    (let [acc (if left
                (.entries left acc)
                acc)
          acc (if right
                (.entries right acc)
                acc)]
      acc))

  (get [this k default]
    (if (p/<= k prefix)
      (.get left k default)
      (.get right k default)))

  (assoc [this k epoch' v]
    (if (match-prefix? k prefix mask)

      (if (off? k mask)
        (if (instance? Empty left)
          (let [n (leaf k epoch' v)]
            (if (p/== epoch epoch')
              (do (set! left n) this)
              (branch prefix mask epoch' n right)))
          (let [left' (.assoc left k epoch' v)]
            (if (identical? left left')
              this
              (branch prefix mask epoch' left' right))))
        (if (instance? Empty right)
          (let [n (leaf k epoch' v)]
            (if (p/== epoch epoch')
              (do (set! right n) this)
              (branch prefix mask epoch' left n)))
          (let [right' (.assoc right k epoch' v)]
            (if (identical? right right')
              this
              (branch prefix mask epoch' left right')))))

      (join epoch'
        k mask (leaf k epoch v)
        prefix mask this)))

  (dissoc [this k epoch']
    (if (p/<= k prefix)
      (let [left' (.dissoc left k epoch')]
        (if (identical? left left')
          this
          (branch prefix mask epoch' left' right)))
      (let [right' (.dissoc right k epoch')]
        (if (identical? right right')
          this
          (branch prefix mask epoch' left right')))))

  (toString [_]
    (pr-str
      {:prefix prefix
       :mask mask
       :epoch epoch
       :left (when left (read-string (str left)))
       :right (when right (read-string (str right)))})))

(deftype Leaf
  [^long key
   ^long epoch
   ^:volatile-mutable value]
  INode
  (entries [_ acc]
    (conj! acc (clojure.lang.MapEntry. key value)))
  (get [_ ^long k default]
    (if (p/== key k)
      value
      default))
  (dissoc [this k epoch']
    (if (p/== key k)
      (Empty.)
      this))
  (assoc [this k epoch' v]
    (if (p/== k key)
      (if (p/== epoch epoch')
        (do
          (set! value v)
          this)
        (Leaf. k epoch' v))
      (join epoch'
        k Long/MIN_VALUE (Leaf. k epoch' v)
        key Long/MIN_VALUE this)))
  (toString [_]
    (pr-str {:key key :epoch epoch :value value})))

(defn- leaf [^long key ^long epoch value]
  (Leaf. key epoch value))

(defn- branch [prefix mask epoch left right]
  (Branch. (bit-mask (p/long prefix) (p/long mask)) mask epoch left right))

;;;

(defmacro ^:private compile-if [test then else]
  (if (eval test)
    then
    else))

(declare ->transient)

(deftype PersistentIntMap
  [^INode root
   ^long epoch
   meta]

  clojure.lang.IObj
  (meta [_] meta)
  (withMeta [_ m] (PersistentIntMap. root epoch m))

  clojure.lang.MapEquivalence

  clojure.lang.Counted
  (count [this]
    (count (seq this)))

  clojure.lang.IPersistentCollection

  (equiv [this x]
    (and (map? x) (= x (into {} this))))

  (cons [this o]
    (if (map? o)
      (reduce #(apply assoc %1 %2) this o)
      (.assoc this (nth o 0) (nth o 1))))

  clojure.lang.Seqable
  (seq [this]
    (seq (persistent! (.entries root (transient [])))))

  clojure.core.protocols.CollReduce

  (coll-reduce
    [this f]
    (reduce f (seq this)))

  (coll-reduce
    [this f val#]
    (reduce f val# (seq this)))

  Object
  (hashCode [this]
    (reduce
      (fn [acc [k v]]
        (unchecked-add acc (bit-xor (.hashCode k) (.hashCode v))))
      0
      (seq this)))

  clojure.lang.IHashEq
  (hasheq [this]
    (compile-if (resolve 'clojure.core/hash-unordered-coll)
      (hash-unordered-coll this)
      (.hashCode this)))

  (equals [this x]
    (or (identical? this x)
      (and
        (map? x)
        (= x (into {} this)))))

  (toString [this]
    (str (into {} this)))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k default]
    (.get root (long k) default))

  clojure.lang.Associative
  (containsKey [this k]
    (not (identical? ::not-found (.valAt this k ::not-found))))

  (entryAt [this k]
    (let [v (.valAt this k ::not-found)]
      (when (not= v ::not-found)
        (clojure.lang.MapEntry. k v))))

  (assoc [this k v]
    (let [k (long k)
          epoch' (p/inc epoch)]
      (when (p/< k 0)
        (throw (IllegalArgumentException. "int-map can only have positive integers as keys")))
      (PersistentIntMap.
        (.assoc root (long k) epoch' v)
        epoch'
        meta)))

  (empty [this]
    (PersistentIntMap. (Empty.) 0 nil))

  clojure.lang.IEditableCollection
  (asTransient [this]
    (->transient root epoch meta))

  java.util.Map
  (get [this k]
    (.valAt this k))
  (isEmpty [this]
    (empty? (seq this)))
  (size [this]
    (count this))
  (keySet [_]
    (->> (.entries root [])
      (map key)
      set))
  (put [_ _ _]
    (throw (UnsupportedOperationException.)))
  (putAll [_ _]
    (throw (UnsupportedOperationException.)))
  (clear [_]
    (throw (UnsupportedOperationException.)))
  (remove [_ _]
    (throw (UnsupportedOperationException.)))
  (values [this]
    (->> this seq (map second)))
  (entrySet [this]
    (->> this seq set))
  (iterator [this]
    (clojure.lang.SeqIterator. (seq this)))

  clojure.lang.IPersistentMap
  (assocEx [this k v]
    (if (contains? this k)
      (throw (Exception. "Key or value already present"))
      (assoc this k v)))
  (without [this k]
    (let [k (long k)
          epoch' (p/inc epoch)]
      (PersistentIntMap.
        (or
          (.dissoc root k epoch')
          (Empty.))
        epoch'
        meta)))

  clojure.lang.IFn

  (invoke [this k]
    (.valAt this k))

  (invoke [this k default]
    (.valAt this k default)))

(deftype TransientIntMap
  [^INode root
   ^long epoch
   meta]

  clojure.lang.IObj
  (meta [_] meta)
  (withMeta [_ m] (TransientIntMap. root (p/inc epoch) meta))

  clojure.lang.Counted
  (count [this]
    (count (seq this)))

  clojure.lang.MapEquivalence

  (equiv [this x]
    (and (map? x) (= x (into {} this))))

  clojure.lang.Seqable
  (seq [this]
    (seq (persistent! (.entries root (transient [])))))

  Object
  (hashCode [this]
    (reduce
      (fn [acc [k v]]
        (unchecked-add acc (bit-xor (hash k) (hash v))))
      0
      (seq this)))

  (equals [this x]
    (or (identical? this x)
      (and
        (map? x)
        (= x (into {} this)))))

  (toString [this]
    (str (into {} this)))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k default]
    (.get root k default))

  clojure.lang.Associative
  (containsKey [this k]
    (not (identical? ::not-found (.valAt this k ::not-found))))

  (entryAt [this k]
    (let [v (.valAt this k ::not-found)]
      (when (not= v ::not-found)
        (clojure.lang.MapEntry. k v))))

  clojure.lang.ITransientMap

  (assoc [this k v]
    (let [k (long k)
          _ (when (p/< k 0)
              (throw (IllegalArgumentException. "int-map can only have positive integers as keys")))
          root' (.assoc root (long k) epoch v)]
      (if (identical? root' root)
        this
        (TransientIntMap. root' epoch meta))))

  (conj [this o]
    (if (map? o)
      (reduce #(apply assoc! %1 %2) this o)
      (.assoc this (nth o 0) (nth o 1))))

  (persistent [_]
    (PersistentIntMap. root (p/inc epoch) meta))

  (without [this k]
    (let [root' (.dissoc root (long k) epoch)]
      (if (identical? root' root)
        this
        (TransientIntMap. root' epoch meta))))

  clojure.lang.IFn

  (invoke [this k]
    (.valAt this k))

  (invoke [this k default]
    (.valAt this k default)))

(defn- ->transient [root ^long epoch meta]
  (TransientIntMap. root epoch meta))

(defn int-map
  ([]
     (PersistentIntMap. (Empty.) 0 nil)))

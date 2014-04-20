(ns
  ^{:doc "A straightforward port of Okasaki and Gill's \"Fast Mergeable Integer Maps`\",
          which can be found at http://ittc.ku.edu/~andygill/papers/IntMap98.pdf"}
  immutable-int-map
  (:refer-clojure :exclude [merge merge-with])
  (:require [clojure.core #_primitive-math :as p]))

(definterface INode
  (^long count [])
  (merge [node epoch f])
  (entries [acc])
  (assoc [^long k ^long epoch f v])
  (dissoc [^long k ^long epoch])
  (update [^long k ^long epoch f])
  (get [^long k default]))

(set! *warn-on-reflection* false)
(set! *unchecked-math* true)

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

(defmacro ^:private branch [prefix mask epoch left right]
  `(let [left?# (instance? ~'Empty ~left)
         right?# (instance? ~'Empty ~right)]
     (cond
       (and left?# right?#)
       (new ~'Empty)

       left?#
       ~right

       right?#
       ~left

       :else
       (new ~'Branch
         (bit-mask ~prefix ~mask)
         ~mask
         ~epoch
         (p/+
           (.count ~(with-meta left {:tag "INode"}))
           (.count ~(with-meta right {:tag "INode"})))
         ~left
         ~right))))

(defmacro ^:private join [epoch p0 m0 t0 p1 m1 t1]
  `(let [m# (branching-bit ~p0 ~m0 ~p1 ~m1)]
     (if (off? ~p0 m#)
       (branch ~p0 m# ~epoch ~t0 ~t1)
       (branch ~p0 m# ~epoch ~t1 ~t0))))

(defn ^:private default-merge [_ x]
  x)

;;;

(declare leaf)

(set! *warn-on-reflection* true)

(deftype Empty []
  INode
  (update [_ k epoch f]
    (leaf k epoch (f nil)))
  (merge [_ n epoch f]
    n)
  (count [_]
    0)
  (entries [_ acc]
    acc)
  (get [_ _ default]
    default)
  (dissoc [this _ _]
    this)
  (assoc [_ k epoch f v]
    (leaf k epoch v)))

(deftype Branch
  [^long prefix
   ^long mask
   ^long epoch
   ^long count
   ^INode left
   ^INode right]
  INode

  (update [this k epoch' f]
    (if (p/<= k prefix)
      (let [left' (.update left k epoch' f)]
        (if (identical? left left')
          this
          (branch prefix mask epoch' left' right)))
      (let [right' (.update right k epoch' f)]
        (if (identical? right right')
          this
          (branch prefix mask epoch' left right')))))

  (merge [this n epoch' f]
    (if (instance? Branch n)

      (let [^Branch n n]
        (cond

          ;; merge subtrees
          (and
            (p/== (.prefix n) prefix)
            (p/== (.mask n) mask))
          (branch prefix mask epoch'
            (.merge left (.left n) epoch' f)
            (.merge right (.right n) epoch' f))

          ;; we contain the other node
          (and (p/> mask (.mask n)) (match-prefix? (.prefix n) prefix mask))
          (if (off? (.prefix n) mask)
            (branch prefix mask epoch' (.merge left n epoch' f) right)
            (branch prefix mask epoch' left (.merge right n epoch' f)))

          ;; the other node contains us
          (and (p/< mask (.mask n)) (match-prefix? prefix (.prefix n) (.mask n)))
          (if (off? prefix (.mask n))
            (branch (.prefix n) (.mask n) epoch' (.merge this (.left n) epoch' f) (.right n))
            (branch (.prefix n) (.mask n) epoch' (.left n) (.merge this (.right n) epoch' f)))

          :else
          (join epoch'
            prefix mask this
            (.prefix n) (.mask n) n)))

      ;; not a branch, let the other node's logic handle it
      (.merge ^INode n this epoch' (fn [x y] (f y x)))))

  (count [_]
    count)

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

  (assoc [this k epoch' f v]
    (if (match-prefix? k prefix mask)

      (if (off? k mask)
        (let [left' (.assoc left k epoch' f v)]
          (if (identical? left left')
            this
            (branch prefix mask epoch' left' right)))
        (let [right' (.assoc right k epoch' f v)]
          (if (identical? right right')
            this
            (branch prefix mask epoch' left right'))))

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
  (update [this k epoch' f]
    (if (p/== k key)
      (let [value' (f value)]
        (if (p/== epoch epoch')
          (do (set! value value') this)
          (Leaf. key epoch' value')))
      (.assoc this k epoch' nil (f nil))))
  (merge [this n epoch' f]
    (.assoc ^INode n key epoch' f value))
  (count [_]
    1)
  (entries [_ acc]
    (.add ^java.util.ArrayList acc (clojure.lang.MapEntry. key value))
    acc)
  (get [_ k default]
    (if (p/== key k)
      value
      default))
  (dissoc [this k epoch']
    (if (p/== key k)
      (Empty.)
      this))
  (assoc [this k epoch' f v]
    (if (p/== k key)
      (let [value' (f value v)]
        (if (p/== epoch epoch')
          (do
            (set! value value')
            this)
          (Leaf. k epoch' value')))
      (join epoch'
        k 0 (Leaf. k epoch' v)
        key 0 this)))
  (toString [_]
    (pr-str {:key key :epoch epoch :value value})))

(defn- leaf [^long key ^long epoch value]
  (Leaf. key epoch value))

;;;

(definterface IRadix
  (mergeWith [b f])
  (update [k f]))

(defmacro ^:private compile-if [test then else]
  (if (eval test)
    then
    else))

(declare ->transient)

(deftype PersistentIntMap
  [^INode root
   ^long epoch
   meta]

  IRadix
  (mergeWith [_ b f]
    (let [^PersistentIntMap b b
          epoch' (p/inc (p/max (.epoch b) epoch))]
      (PersistentIntMap.
        (.merge root (.root b) epoch' f)
        epoch'
        meta)))

  (update [_ k f]
    (let [epoch' (p/inc epoch)
          root' (.update root k epoch' f)]
      (PersistentIntMap. root' epoch' meta)))

  clojure.lang.IObj
  (meta [_] meta)
  (withMeta [_ m] (PersistentIntMap. root epoch m))

  clojure.lang.MapEquivalence

  clojure.lang.Counted
  (count [this]
    (.count root))

  clojure.lang.IPersistentCollection

  (equiv [this x]
    (and (map? x) (= x (into {} this))))

  (cons [this o]
    (if (map? o)
      (reduce #(apply assoc %1 %2) this o)
      (.assoc this (nth o 0) (nth o 1))))

  clojure.lang.Seqable
  (seq [this]
    (seq (.entries root (java.util.ArrayList.))))

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
        (throw (IllegalArgumentException. "int-map can only have non-negative integers as keys")))
      (PersistentIntMap.
        (.assoc root (long k) epoch' default-merge v)
        epoch'
        meta)))

  (empty [this]
    (PersistentIntMap. (Empty.) 0 nil))

  clojure.lang.IEditableCollection
  (asTransient [this]
    (->transient root (p/inc epoch) meta))

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

  IRadix
  (mergeWith [this b f]
    (throw (IllegalArgumentException. "Cannot call `merge-with` on transient int-map.")))

  (update [this k f]
    (let [root' (.update root k epoch f)]
      (if (identical? root root')
        this
        (TransientIntMap. root' epoch meta))))

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
              (throw (IllegalArgumentException. "int-map can only have non-negative integers as keys")))
          root' (.assoc root (long k) epoch default-merge v)]
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

;;;

(defn int-map
  "Creates an integer map that can only have non-negative integers as keys."
  ([]
     (PersistentIntMap. (Empty.) 0 nil))
  ([a b]
     (assoc (int-map) a b))
  ([a b & rest]
     (apply assoc (int-map) a b rest)))

(defn merge-with
  "Merges together two int-maps, using `f` to resolve value conflicts."
  ([f a b]
     (.mergeWith ^IRadix a b f))
  ([f a b & rest]
     (reduce #(merge-with f % %) (list* a b rest))))

(defn merge
  "Merges together two int-maps, giving precedence to values from the right-most map."
  ([a b]
     (merge-with (fn [_ b] b) a b))
  ([a b & rest]
     (apply merge-with (fn [_ b] b) a b rest)))

(defn update
  "Updates the value associated with the given key.  If no such key exist, `f` is invoked
   with `nil`."
  ([m k f]
     (.update ^PersistentIntMap m k f))
  ([m k f & args]
     (update m k #(apply f % args))))

(defn update!
  "A transient variant of `update`."
  ([m k f]
     (.update ^TransientIntMap m k f))
  ([m k f & args]
     (update! m k #(apply f % args))))

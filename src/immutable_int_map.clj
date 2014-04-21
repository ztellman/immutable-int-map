(ns
  ^{:doc "A straightforward port of Okasaki and Gill's \"Fast Mergeable Integer Maps`\",
          which can be found at http://ittc.ku.edu/~andygill/papers/IntMap98.pdf"}
  immutable-int-map
  (:refer-clojure
    :exclude [merge merge-with])
  (:require
    [clojure.core.reducers :as r]
    [immutable-int-map.nodes :as n]
    [clojure.core #_primitive-math :as p])
  (:import
    [immutable_int_map.nodes
     INode
     Empty]))

;;;

(defn ^:private default-merge [_ x]
  x)

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

  r/CollFold

  (coll-fold [this n combinef reducef]
    (r/coll-fold root n combinef reducef))

  clojure.core.protocols.CollReduce

  (coll-reduce
    [this f]
    (let [x (reduce f root)]
      (if (reduced? x)
        @x
        x)))

  (coll-reduce
    [this f val]
    (let [x (reduce f val root)]
      (if (reduced? x)
        @x
        x)))

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
    (.count root))

  clojure.lang.MapEquivalence

  (equiv [this x]
    (and (map? x) (= x (into {} this))))

  clojure.lang.Seqable
  (seq [this]
    (seq (.entries root (java.util.ArrayList.))))

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
  ([f]
     (int-map))
  ([f a b]
     (let [a' (if (instance? TransientIntMap a)
                (persistent! a)
                a)
           b'  (if (instance? TransientIntMap b)
                 (persistent! b)
                 b)]
       (.mergeWith ^IRadix a' b' f)))
  ([f a b & rest]
     (reduce #(merge-with f %1 %2) (list* a b rest))))

(defn merge
  "Merges together two int-maps, giving precedence to values from the right-most map."
  ([]
     (int-map))
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

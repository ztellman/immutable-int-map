(ns immutable-int-map-test
  (:use
    [clojure.test])
  (:require
    [clojure.core.reducers :as r]
    [immutable-int-map :as i]
    [collection-check :as check]
    [criterium.core :as c]
    [simple-check.generators :as gen]
    [simple-check.properties :as prop]
    [simple-check.clojure-test :as ct :refer (defspec)]))

;;;

(deftest test-map-like
  (check/assert-map-like 1e3 (i/int-map) gen/pos-int gen/int))

(def int-map-generator
  (gen/fmap
    (fn [ks]
      (into (i/int-map) ks))
    (gen/list (gen/tuple gen/pos-int gen/int))))

(defspec equivalent-update 1e3
    (let [f #(if % (inc %) 1)]
      (prop/for-all [m int-map-generator k gen/pos-int]
        (= (i/update m k f)
          (assoc m k (f (get m k)))))))

(defspec equivalent-merge 1e3
  (prop/for-all [a int-map-generator, b int-map-generator]
    (= (merge-with + a b) (i/merge-with + a b))))

;;;

(def n (long 1e6))

(def ordered-entries (vec (map vector (range n) (range n))))
(def entries (vec (shuffle ordered-entries)))

(deftest ^:benchmark benchmark-maps

  (println "into {} unordered")
  (c/quick-bench
    (into {} entries))

  (println "into {} ordered")
  (c/quick-bench
    (into {} ordered-entries))

  (println "into (sorted-map) unordered")
  (c/quick-bench
    (into (sorted-map) entries))

  (println "into (sorted-map) ordered")
  (c/quick-bench
    (into (sorted-map) ordered-entries))

  (println "into (int-map) unordered")
  (c/quick-bench
    (into (i/int-map) entries))

  (println "into (int-map) ordered")
  (c/quick-bench
    (into (i/int-map) ordered-entries))

  (println "into (int-map) fold/merge unordered")
  (c/quick-bench
    (r/fold i/merge conj entries))

  (println "into (int-map) fold/merge ordered")
  (c/quick-bench
    (r/fold i/merge conj ordered-entries))

  (let [m (into {} entries)]
    (println "get {}")
    (c/quick-bench
      (get m 1e3)))

  (let [m (into (i/int-map) entries)]
    (println "get (int-map)")
    (c/quick-bench
      (get m 1e3)))

  (let [m (into (sorted-map) entries)]
    (println "get (sorted-map)")
    (c/quick-bench
      (get m 1e3))))

(ns immutable-int-map-test
  (:use
    [clojure.test])
  (:require
    [immutable-int-map :as i]
    [collection-check :as check]
    [criterium.core :as c]
    [simple-check.generators :as gen]
    [simple-check.properties :as prop]
    [simple-check.clojure-test :as ct :refer (defspec)]))

(deftest test-map-like
  (check/assert-map-like 1e3 (i/int-map) gen/pos-int gen/int))

(def entries (vec (map vector (range 1e6) (range 1e6))))

(deftest ^:benchmark benchmark-maps
  (println "assoc values")
  (c/quick-bench
    (into {} entries))

  (println "into int-map 1e3 entries")
  (c/quick-bench
    (into (i/int-map) entries)))

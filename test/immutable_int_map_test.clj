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
  (check/assert-map-like 1e4 (i/int-map) gen/pos-int gen/int))

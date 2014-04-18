(defproject immutable-int-map "0.1.0-SNAPSHOT"
  :description "an immutable map of integers onto values"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[primitive-math "0.1.3"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.6.0"]
                                  [collection-check "0.1.2"]
                                  [criterium "0.4.3"]]}}
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark}
  :jvm-opts ["-server"])

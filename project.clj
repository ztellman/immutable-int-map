(defproject immutable-int-map "0.1.0"
  :description "an immutable map of integers onto values"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies []
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.6.0"]
                                  [collection-check "0.1.2"]
                                  [criterium "0.4.3"]
                                  [primitive-math "0.1.3"]
                                  [codox-md "0.2.0" :exclusions [org.clojure/clojure]]]}}
  :plugins [[codox "0.6.6"]]
  :codox {:writer codox-md.writer/write-docs}
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark}
  :jvm-opts ["-server"])

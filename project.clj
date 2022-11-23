(defproject graphqlfmt "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [instaparse "1.4.10"]]
  :main graphqlfmt.core
  :aot :all
  :resource-paths ["resources"]
  :test-paths ["test" "test/resources"]
  :profiles {:dev {:dependencies [[com.clojure-goes-fast/clj-async-profiler "1.0.3"]
                                  [criterium "0.4.6"]]
                   :source-paths ["demo"]
                   :resource-paths ["test/resources"]}})

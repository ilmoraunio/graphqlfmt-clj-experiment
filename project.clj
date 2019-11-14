(defproject graphql-fmt "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [instaparse "1.4.10"]]
  :main graphql-fmt.core
  :aot :all
  :resource-paths ["resources"]
  :test-paths ["test"])

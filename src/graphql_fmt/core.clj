(ns graphql-fmt.core
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta])
  (:gen-class))

(def graphql-parser
  (insta/parser
    (-> "graphql.ebnf"
      io/resource
      io/reader
      slurp)))

(defn -main [& _args]
  (println (graphql-parser "{")))

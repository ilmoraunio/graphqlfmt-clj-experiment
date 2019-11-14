(ns graphql-fmt.core
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta])
  (:gen-class))

(def as-and-bs*
  (insta/parser
    (-> "graphql.ebnf"
      io/resource
      io/reader
      slurp)))

(defn -main [& _args]
  (println (as-and-bs* "aaaaabbbaaaabb")))

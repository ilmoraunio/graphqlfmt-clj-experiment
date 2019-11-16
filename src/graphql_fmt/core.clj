(ns graphql-fmt.core
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta])
  (:gen-class))

(def token-parser
  (insta/parser
    (-> "token.ebnf"
      io/resource
      io/reader
      slurp)))

(defn -main [& _args]
  (println (token-parser "{")))

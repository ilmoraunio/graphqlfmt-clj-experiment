(ns graphqlfmt.core
  (:require [graphqlfmt.ast :as ast]
            [graphqlfmt.options :as options]
            [graphqlfmt.print :as print]
            [graphqlfmt.transform :as transform])
  (:gen-class))

(defn fmt
  [s]
  (->> s
    ast/parse
    transform/transform
    options/amend-options
    transform/re-transform
    transform/row-ast
    options/amend-characters-opts
    print/pr-s))

(defn -main [& args]
  (let [graphql (if (>= (count args) 1)
                  (slurp (first args))
                  (slurp *in*))
        output (fmt graphql)]
    (print output)
    (flush)))

(ns graphql-fmt.print-test
  (:refer-clojure :exclude [name comment])
  (:require [clojure.test :refer [are deftest is testing]]
            [graphql-fmt.core :refer :all :as graphqlfmt]
            [instaparse.core :as insta]))

#_(deftest test-unformatted-output
  (are [graphql]
       (let [is-a-vec? (vector? graphql)
             expected (if is-a-vec? (second graphql) graphql)
             input (if is-a-vec? (first graphql) graphql)]
         (amend-indentation-level-opts
           0
           (amend-newline-spacing
             (amend-newline-opts
               (insta/transform
                 transform-map
                 (document-parser input)))))
         (= expected
            (pr-str-ast
              ""
              (insta/transform
                transform-map
                (document-parser input)))))
    ;; from https://graphql.org/learn/schema/:
    "{\n  search(text: \"an\") {\n    __typename\n    ... on Human {\n      name\n      height\n    }\n    ... on Droid {\n      name\n      primaryFunction\n    }\n    ... on Starship {\n      name\n      length\n    }\n  }\n}"
    ))

(def graphql-statements
  (->> (clojure.java.io/file "test-resources/graphql")
    (file-seq)
    (filter #(.isFile %))
    (map (juxt #(.getName %) slurp))
    (mapv (fn [[filename graphql]] {:filename filename
                                    :graphql graphql}))))

(defn- expected
  [filename]
  (when-let [re (re-find #"(.+)\.input(\.graphql)$" filename)]
    (apply str "test-resources/graphql/" (interpose ".expected" (rest re)))))

(defmacro run-tests
  []
  `(do (clojure.template/do-template
         [graphql-statement]
         (let [graphql# (:graphql graphql-statement)]
           (prn (format "testing: %s" (:filename graphql-statement)))
           (if-let [expected# (expected (:filename graphql-statement))]
             (is (= (slurp expected#) (graphqlfmt/fmt graphql#))
                 (:filename graphql-statement))
             (is (= graphql# (graphqlfmt/fmt graphql#))
                 (:filename graphql-statement))))
         ~@graphql-statements)))

(deftest test-formatted-output (run-tests))
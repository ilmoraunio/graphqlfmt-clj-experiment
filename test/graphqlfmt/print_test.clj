(ns graphqlfmt.print-test
  (:refer-clojure :exclude [name comment])
  (:require [clojure.test :refer [are deftest is testing]]
            [graphqlfmt.core :refer :all :as graphqlfmt]
            [instaparse.core :as insta]))

(def graphql-statements
  (->> (clojure.java.io/file "test-resources/graphql")
    (file-seq)
    (filter #(.isFile %))
    (remove #(re-find #"\.expected\." (.getName %)))
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

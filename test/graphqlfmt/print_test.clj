(ns graphqlfmt.print-test
  (:refer-clojure :exclude [name comment])
  (:require [clojure.test :refer [deftest is testing]]
            [graphqlfmt.core :as graphqlfmt]))

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

(deftest test-formatted-output
  (testing "format output is idempotent or formats to .expected.graphql"
    (doseq [graphql-statement graphql-statements]
      (let [graphql (:graphql graphql-statement)]
        (prn (format "testing: %s" (:filename graphql-statement)))
        (if-let [expected-output (expected (:filename graphql-statement))]
          (is (= (slurp expected-output) (graphqlfmt/fmt graphql))
              (:filename graphql-statement))
          (is (= graphql (graphqlfmt/fmt graphql))
              (:filename graphql-statement)))))))

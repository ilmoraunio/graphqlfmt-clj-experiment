(ns graphql-fmt.core-test
  (:require [clojure.test :refer [are deftest is testing]]
            [graphql-fmt.core :as core]))

(deftest test-example
  (is (= [:S
          [:AB [:A "a" "a" "a" "a" "a"] [:B "b" "b" "b"]]
          [:AB [:A "a" "a" "a" "a"] [:B "b" "b"]]]
         (core/as-and-bs* "aaaaabbbaaaabb"))))

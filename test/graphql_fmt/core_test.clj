(ns graphql-fmt.core-test
  (:require [clojure.test :refer [are deftest is testing]]
            [graphql-fmt.core :as core]))

(deftest test-example
  (is (= [:Punctuator "{"]
         (core/graphql-parser "{"))))

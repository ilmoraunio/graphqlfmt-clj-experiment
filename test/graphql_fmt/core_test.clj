(ns graphql-fmt.core-test
  (:require [clojure.test :refer [are deftest is testing]]
            [graphql-fmt.core :as core]))

(deftest test-tokens
  (is (= [:Punctuator "{"]
         (core/graphql-parser "{")))
  (is (= [:Token [:Name "frob"]]
         (core/graphql-parser "frob"))))

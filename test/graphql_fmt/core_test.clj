(ns graphql-fmt.core-test
  (:require [clojure.test :refer [are deftest is testing]]
            [graphql-fmt.core :refer [graphql-parser]]))

(deftest test-tokens
  (is (= [:Token [:Punctuator "{"]]
         (graphql-parser "{")))
  (is (= [:Token [:Name "frob"]]
         (graphql-parser "frob")))
  (is (= [:Token [:IntValue [:IntegerPart "0"]]]
         (graphql-parser "0")))
  (is (= [:Token [:IntValue [:IntegerPart [:NegativeSign "-"] "0"]]]
         (graphql-parser "-0")))
  (is (= [:Token [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]
         (graphql-parser "1")))
  (is (= [:Token [:IntValue [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"]]]]
         (graphql-parser "-1")))
  (is (= [:Token [:IntValue [:IntegerPart [:NonZeroDigit "1"] [:Digit "0"]]]]
         (graphql-parser "10")))
  (is (= [:Token
          [:IntValue [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"] [:Digit "0"]]]]
         (graphql-parser "-10"))))

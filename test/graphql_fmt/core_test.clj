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
         (graphql-parser "-10")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NonZeroDigit "1"]]
           [:FractionalPart "." [:Digit "0"]]]]
         (graphql-parser "1.0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"]]
           [:FractionalPart "." [:Digit "0"]]]]
         (graphql-parser "-1.0")))
  (is (= [:Token [:FloatValue [:IntegerPart "0"] [:FractionalPart "." [:Digit "1"]]]]
         (graphql-parser "0.1")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:FractionalPart "." [:Digit "1"]]]]
         (graphql-parser "-0.1")))
  (is (= [:Token [:FloatValue [:IntegerPart "0"] [:FractionalPart "." [:Digit "0"]]]]
         (graphql-parser "0.0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:FractionalPart "." [:Digit "0"]]]]
         (graphql-parser "-0.0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NonZeroDigit "1"]]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "5"] [:Digit "0"]]]]
         (graphql-parser "1e50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "5"] [:Digit "0"]]]]
         (graphql-parser "0e50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "0"]]]]
         (graphql-parser "0e0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "0"]]]]
         (graphql-parser "-0e0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "5"] [:Digit "0"]]]]
         (graphql-parser "-0e50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"]]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "5"] [:Digit "0"]]]]
         (graphql-parser "-1e50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "0"]]]]
         (graphql-parser "0e-0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "5"] [:Digit "0"]]]]
         (graphql-parser "0e-50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NonZeroDigit "1"]]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "5"] [:Digit "0"]]]]
         (graphql-parser "1e-50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "0"]]]]
         (graphql-parser "-0e-0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "5"] [:Digit "0"]]]]
         (graphql-parser "-0e-50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"]]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "5"] [:Digit "0"]]]]
         (graphql-parser "-1e-50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NonZeroDigit "6"]]
           [:FractionalPart
            "."
            [:Digit "0"]
            [:Digit "2"]
            [:Digit "2"]
            [:Digit "1"]
            [:Digit "4"]
            [:Digit "1"]
            [:Digit "3"]]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "2"] [:Digit "3"]]]]
         (graphql-parser "6.0221413e23")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "6"]]
           [:FractionalPart
            "."
            [:Digit "0"]
            [:Digit "2"]
            [:Digit "2"]
            [:Digit "1"]
            [:Digit "4"]
            [:Digit "1"]
            [:Digit "3"]]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "2"] [:Digit "3"]]]]
         (graphql-parser "-6.0221413e-23")))
  (is (= [:Token [:StringValue]] (graphql-parser "\"\"")))
  (is (= [:Token [:StringValue [:StringCharacter "*"]]]
         (graphql-parser "\"*\"")))
  (is (= [:Token
          [:StringValue
           [:StringCharacter "f"]
           [:StringCharacter "r"]
           [:StringCharacter "o"]
           [:StringCharacter "b"]]]
         (graphql-parser "\"frob\"")))
  (is (= [:Token
          [:StringValue [:StringCharacter "\\u" [:EscapedUnicode "0000"]]]]
         (graphql-parser "\"\\u0000\"")))
  (is (= [:Token
          [:StringValue
           [:StringCharacter "\\" [:EscapedCharacter "r"]]
           [:StringCharacter "\\" [:EscapedCharacter "n"]]]]
        (graphql-parser "\"\\r\\n\"")))
  (is (= [:Token
          [:StringValue
           [:BlockStringCharacter " "]
           [:BlockStringCharacter "f"]
           [:BlockStringCharacter "r"]
           [:BlockStringCharacter "o"]
           [:BlockStringCharacter "b"]
           [:BlockStringCharacter "\""]
           [:BlockStringCharacter "f"]
           [:BlockStringCharacter "r"]
           [:BlockStringCharacter "o"]
           [:BlockStringCharacter "b"]
           [:BlockStringCharacter "n"]
           [:BlockStringCharacter "i"]
           [:BlockStringCharacter "t"]
           [:BlockStringCharacter "z"]
           [:BlockStringCharacter "\"\""]
           [:BlockStringCharacter "f"]
           [:BlockStringCharacter "r"]
           [:BlockStringCharacter "o"]
           [:BlockStringCharacter "b"]
           [:BlockStringCharacter "n"]
           [:BlockStringCharacter "i"]
           [:BlockStringCharacter "c"]
           [:BlockStringCharacter "a"]
           [:BlockStringCharacter "t"]
           [:BlockStringCharacter "e"]
           [:BlockStringCharacter " "]]]
         (graphql-parser "\"\"\" frob\"frobnitz\"\"frobnicate \"\"\"")))
  (is (instance? instaparse.gll.Failure
                 (graphql-parser "\"\"\" \"\"\" \"\"\"")))
  (is (= [:Token
          [:StringValue
           [:BlockStringCharacter " "]
           [:BlockStringCharacter "\"\"\""]
           [:BlockStringCharacter " "]]]
         (graphql-parser "\"\"\" \\\"\"\" \"\"\""))))

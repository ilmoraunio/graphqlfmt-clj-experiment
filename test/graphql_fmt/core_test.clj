(ns graphql-fmt.core-test
  (:require [clojure.test :refer [are deftest is testing]]
            [graphql-fmt.core :refer [token-parser]]))

(deftest test-tokens
  (is (= [:Token [:Punctuator "{"]]
         (token-parser "{")))
  (is (= [:Token [:Name "frob"]]
         (token-parser "frob")))
  (is (= [:Token [:IntValue [:IntegerPart "0"]]]
         (token-parser "0")))
  (is (= [:Token [:IntValue [:IntegerPart [:NegativeSign "-"] "0"]]]
         (token-parser "-0")))
  (is (= [:Token [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]
         (token-parser "1")))
  (is (= [:Token [:IntValue [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"]]]]
         (token-parser "-1")))
  (is (= [:Token [:IntValue [:IntegerPart [:NonZeroDigit "1"] [:Digit "0"]]]]
         (token-parser "10")))
  (is (= [:Token
          [:IntValue [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"] [:Digit "0"]]]]
         (token-parser "-10")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NonZeroDigit "1"]]
           [:FractionalPart "." [:Digit "0"]]]]
         (token-parser "1.0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"]]
           [:FractionalPart "." [:Digit "0"]]]]
         (token-parser "-1.0")))
  (is (= [:Token [:FloatValue [:IntegerPart "0"] [:FractionalPart "." [:Digit "1"]]]]
         (token-parser "0.1")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:FractionalPart "." [:Digit "1"]]]]
         (token-parser "-0.1")))
  (is (= [:Token [:FloatValue [:IntegerPart "0"] [:FractionalPart "." [:Digit "0"]]]]
         (token-parser "0.0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:FractionalPart "." [:Digit "0"]]]]
         (token-parser "-0.0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NonZeroDigit "1"]]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "5"] [:Digit "0"]]]]
         (token-parser "1e50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "5"] [:Digit "0"]]]]
         (token-parser "0e50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "0"]]]]
         (token-parser "0e0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "0"]]]]
         (token-parser "-0e0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "5"] [:Digit "0"]]]]
         (token-parser "-0e50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"]]
           [:ExponentPart [:ExponentIndicator "e"] [:Digit "5"] [:Digit "0"]]]]
         (token-parser "-1e50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "0"]]]]
         (token-parser "0e-0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "5"] [:Digit "0"]]]]
         (token-parser "0e-50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NonZeroDigit "1"]]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "5"] [:Digit "0"]]]]
         (token-parser "1e-50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "0"]]]]
         (token-parser "-0e-0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "5"] [:Digit "0"]]]]
         (token-parser "-0e-50")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"]]
           [:ExponentPart [:ExponentIndicator "e"] [:Sign "-"] [:Digit "5"] [:Digit "0"]]]]
         (token-parser "-1e-50")))
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
         (token-parser "6.0221413e23")))
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
         (token-parser "-6.0221413e-23")))
  (is (= [:Token [:StringValue]] (token-parser "\"\"")))
  (is (= [:Token [:StringValue [:StringCharacter "*"]]]
         (token-parser "\"*\"")))
  (is (= [:Token
          [:StringValue
           [:StringCharacter "f"]
           [:StringCharacter "r"]
           [:StringCharacter "o"]
           [:StringCharacter "b"]]]
         (token-parser "\"frob\"")))
  (is (= [:Token
          [:StringValue [:StringCharacter "\\u" [:EscapedUnicode "0000"]]]]
         (token-parser "\"\\u0000\"")))
  (is (= [:Token
          [:StringValue
           [:StringCharacter "\\" [:EscapedCharacter "r"]]
           [:StringCharacter "\\" [:EscapedCharacter "n"]]]]
         (token-parser "\"\\r\\n\"")))
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
         (token-parser "\"\"\" frob\"frobnitz\"\"frobnicate \"\"\"")))
  (is (instance? instaparse.gll.Failure
                 (token-parser "\"\"\" \"\"\" \"\"\"")))
  (is (= [:Token
          [:StringValue
           [:BlockStringCharacter " "]
           [:BlockStringCharacter "\"\"\""]
           [:BlockStringCharacter " "]]]
         (token-parser "\"\"\" \\\"\"\" \"\"\""))))

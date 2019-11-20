(ns graphql-fmt.core-test
  (:require [clojure.test :refer [are deftest is testing]]
            [graphql-fmt.core :refer [document-parser
                                      ignored-parser
                                      token-parser]]))

(deftest test-ignored
  (are [input ast]
       (= ast (ignored-parser input))

    (str \uFEFF)
    [:Ignored [:UnicodeBOM (str \uFEFF)]]

    (str \u0009)
    [:Ignored [:WhiteSpace (str \u0009)]]

    (str \u0020)
    [:Ignored [:WhiteSpace (str \u0020)]]

    "\n"
    [:Ignored [:LineTerminator [:NewLine "\n"]]]

    "\r"
    [:Ignored [:LineTerminator [:NewLine "\r"]]]

    "\r\n"
    [:Ignored [:LineTerminator [:NewLine "\r" "\n"]]]

    "#"
    [:Ignored [:Comment]]

    "# frobnitz"
    [:Ignored
     [:Comment
      [:CommentChar " "]
      [:CommentChar "f"]
      [:CommentChar "r"]
      [:CommentChar "o"]
      [:CommentChar "b"]
      [:CommentChar "n"]
      [:CommentChar "i"]
      [:CommentChar "t"]
      [:CommentChar "z"]]]

    ","
    [:Ignored [:Comma]]))

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
           [:FractionalPart [:Digit "0"]]]]
         (token-parser "1.0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] [:NonZeroDigit "1"]]
           [:FractionalPart [:Digit "0"]]]]
         (token-parser "-1.0")))
  (is (= [:Token [:FloatValue [:IntegerPart "0"] [:FractionalPart [:Digit "1"]]]]
         (token-parser "0.1")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:FractionalPart [:Digit "1"]]]]
         (token-parser "-0.1")))
  (is (= [:Token [:FloatValue [:IntegerPart "0"] [:FractionalPart [:Digit "0"]]]]
         (token-parser "0.0")))
  (is (= [:Token
          [:FloatValue
           [:IntegerPart [:NegativeSign "-"] "0"]
           [:FractionalPart [:Digit "0"]]]]
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
         (token-parser "\"\"\" \\\"\"\" \"\"\"")))
  (is (= [:Token [:BooleanValue "true"]]
        (token-parser "true")))
  (is (= [:Token [:BooleanValue "false"]]
         (token-parser "false")))
  (is (= [:Token [:NullValue]]
         (token-parser "null"))))

(deftest test-document
  (are [document ast]
       (is (= ast (document-parser document)))

       "{foo}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition [:SelectionSet [:Selection [:Field [:Name "foo"]]]]]]]]

       "{foo_alias:foo}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection [:Field [:Alias [:Name "foo_alias"]] [:Name "foo"]]]]]]]]

       "{foo(bar:$foobar)}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "foo"]
              [:Arguments
               [:Argument [:Name "bar"] [:Value [:Variable [:Name "foobar"]]]]]]]]]]]]

       "{foo(bar:1)}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "foo"]
              [:Arguments
               [:Argument
                [:Name "bar"]
                [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]]]]]]]]]

       "{foo(bar:1.0)}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "foo"]
              [:Arguments
               [:Argument
                [:Name "bar"]
                [:Value
                 [:FloatValue
                  [:IntegerPart [:NonZeroDigit "1"]]
                  [:FractionalPart [:Digit "0"]]]]]]]]]]]]]

       "{foo(bar:\"foobar\")}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "foo"]
              [:Arguments
               [:Argument
                [:Name "bar"]
                [:Value
                 [:StringValue
                  [:StringCharacter "f"]
                  [:StringCharacter "o"]
                  [:StringCharacter "o"]
                  [:StringCharacter "b"]
                  [:StringCharacter "a"]
                  [:StringCharacter "r"]]]]]]]]]]]]

       "{foo(bar:\"\"\"foobar\"\"\")}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "foo"]
              [:Arguments
               [:Argument
                [:Name "bar"]
                [:Value
                 [:StringValue
                  [:BlockStringCharacter "f"]
                  [:BlockStringCharacter "o"]
                  [:BlockStringCharacter "o"]
                  [:BlockStringCharacter "b"]
                  [:BlockStringCharacter "a"]
                  [:BlockStringCharacter "r"]]]]]]]]]]]]

       "{foo(bar:true)}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "foo"]
              [:Arguments [:Argument [:Name "bar"] [:Value [:BooleanValue "true"]]]]]]]]]]]

       "{foo(bar:null)}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "foo"]
              [:Arguments [:Argument [:Name "bar"] [:Value [:NullValue]]]]]]]]]]]

       "{frob(foo:true,bar:false)}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "frob"]
              [:Arguments
               [:Argument [:Name "foo"] [:Value [:BooleanValue "true"]]]
               [:Argument [:Name "bar"] [:Value [:BooleanValue "false"]]]]]]]]]]]

       "{frob @foo}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field [:Name "frob"] [:Directives [:Directive [:Name "foo"]]]]]]]]]]

       "{frob @foo(bar:true)}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "frob"]
              [:Directives
               [:Directive
                [:Name "foo"]
                [:Arguments [:Argument [:Name "bar"] [:Value [:BooleanValue "true"]]]]]]]]]]]]]

       "{frob @foo(a:1,b:2)}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "frob"]
              [:Directives
               [:Directive
                [:Name "foo"]
                [:Arguments
                 [:Argument
                  [:Name "a"]
                  [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]
                 [:Argument
                  [:Name "b"]
                  [:Value [:IntValue [:IntegerPart [:NonZeroDigit "2"]]]]]]]]]]]]]]]))

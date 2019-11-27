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

       "{frob @foo @bar}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "frob"]
              [:Directives [:Directive [:Name "foo"]] [:Directive [:Name "bar"]]]]]]]]]]

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
                  [:Value [:IntValue [:IntegerPart [:NonZeroDigit "2"]]]]]]]]]]]]]]]

       "{frob @foo(a:1) @bar(a:1)}"
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
                  [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]]]
               [:Directive
                [:Name "bar"]
                [:Arguments
                 [:Argument
                  [:Name "a"]
                  [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]]]]]]]]]]]

       "{foo{bar}}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field [:Name "foo"] [:SelectionSet [:Selection [:Field [:Name "bar"]]]]]]]]]]]

       "{foo{bar{foobar}}}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field
              [:Name "foo"]
              [:SelectionSet
               [:Selection
                [:Field
                 [:Name "bar"]
                 [:SelectionSet [:Selection [:Field [:Name "foobar"]]]]]]]]]]]]]]

       "{foo bar}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection [:Field [:Name "foo"]]]
            [:Selection [:Field [:Name "bar"]]]]]]]]

       "{foo{a} bar{a b} foobar{a{b}}}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:Field [:Name "foo"] [:SelectionSet [:Selection [:Field [:Name "a"]]]]]]
            [:Selection
             [:Field
              [:Name "bar"]
              [:SelectionSet
               [:Selection [:Field [:Name "a"]]]
               [:Selection [:Field [:Name "b"]]]]]]
            [:Selection
             [:Field
              [:Name "foobar"]
              [:SelectionSet
               [:Selection
                [:Field [:Name "a"] [:SelectionSet [:Selection [:Field [:Name "b"]]]]]]]]]]]]]]

       "{...frob}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet [:Selection [:FragmentSpread [:FragmentName "frob"]]]]]]]]

       "{...frob @foo}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:FragmentSpread
              [:FragmentName "frob"]
              [:Directives [:Directive [:Name "foo"]]]]]]]]]]

       "{...foo ...bar}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection [:FragmentSpread [:FragmentName "foo"]]]
            [:Selection [:FragmentSpread [:FragmentName "bar"]]]]]]]]

       "{...{frob}}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:InlineFragment [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]]]]

       "{...on Foo{...on Bar{foobar}}}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:InlineFragment
              [:TypeCondition [:NamedType [:Name "Foo"]]]
              [:SelectionSet
               [:Selection
                [:InlineFragment
                 [:TypeCondition [:NamedType [:Name "Bar"]]]
                 [:SelectionSet [:Selection [:Field [:Name "foobar"]]]]]]]]]]]]]]

       "{...on Foo @bar{foobar}}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:SelectionSet
            [:Selection
             [:InlineFragment
              [:TypeCondition [:NamedType [:Name "Foo"]]]
              [:Directives [:Directive [:Name "bar"]]]
              [:SelectionSet [:Selection [:Field [:Name "foobar"]]]]]]]]]]]

       "query{frob}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]

       "mutation{frob}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "mutation"]
           [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]

       "subscription{frob}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "subscription"]
           [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]

       "query frobnicator{frob}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frobnicator"]
           [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]

       "query frobnicator @foo{frob}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frobnicator"]
           [:Directives [:Directive [:Name "foo"]]]
           [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]

       "query frob($foo:bar){a b}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frob"]
           [:VariableDefinitions
            [:VariableDefinition
             [:Variable [:Name "foo"]]
             [:Type [:NamedType [:Name "bar"]]]]]
           [:SelectionSet
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]]]]]]

       "query frob($foo:bar $qux:baz){a b}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frob"]
           [:VariableDefinitions
            [:VariableDefinition
             [:Variable [:Name "foo"]]
             [:Type [:NamedType [:Name "bar"]]]]
            [:VariableDefinition
             [:Variable [:Name "qux"]]
             [:Type [:NamedType [:Name "baz"]]]]]
           [:SelectionSet
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]]]]]]

       "query frob($foo:[bar]){a b}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frob"]
           [:VariableDefinitions
            [:VariableDefinition
             [:Variable [:Name "foo"]]
             [:Type [:ListType [:Type [:NamedType [:Name "bar"]]]]]]]
           [:SelectionSet
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]]]]]]

       "query frob($foo:[[bar]]){a b}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frob"]
           [:VariableDefinitions
            [:VariableDefinition
             [:Variable [:Name "foo"]]
             [:Type [:ListType [:Type [:ListType [:Type [:NamedType [:Name "bar"]]]]]]]]]
           [:SelectionSet
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]]]]]]

       "query frob($foo:bar!){a b}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frob"]
           [:VariableDefinitions
            [:VariableDefinition
             [:Variable [:Name "foo"]]
             [:Type [:NonNullType [:NamedType [:Name "bar"]]]]]]
           [:SelectionSet
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]]]]]]

       "query frob($foo:[bar]!){a b}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frob"]
           [:VariableDefinitions
            [:VariableDefinition
             [:Variable [:Name "foo"]]
             [:Type [:NonNullType [:ListType [:Type [:NamedType [:Name "bar"]]]]]]]]
           [:SelectionSet
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]]]]]]

       "query frob($foo:[bar!]){a b}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frob"]
           [:VariableDefinitions
            [:VariableDefinition
             [:Variable [:Name "foo"]]
             [:Type [:ListType [:Type [:NonNullType [:NamedType [:Name "bar"]]]]]]]]
           [:SelectionSet
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]]]]]]

       "query frob($foo:[bar!]!){a b}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frob"]
           [:VariableDefinitions
            [:VariableDefinition
             [:Variable [:Name "foo"]]
             [:Type
              [:NonNullType
               [:ListType [:Type [:NonNullType [:NamedType [:Name "bar"]]]]]]]]]
           [:SelectionSet
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]]]]]]

       "query frob($foo:bar=true){a b}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:OperationDefinition
           [:OperationType "query"]
           [:Name "frob"]
           [:VariableDefinitions
            [:VariableDefinition
             [:Variable [:Name "foo"]]
             [:Type [:NamedType [:Name "bar"]]]
             [:DefaultValue [:Value [:BooleanValue "true"]]]]]
           [:SelectionSet
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]]]]]]

       "fragment foo on Bar @foobar{a b c}"
       [:Document
        [:Definition
         [:ExecutableDefinition
          [:FragmentDefinition
           [:FragmentName "foo"]
           [:TypeCondition [:NamedType [:Name "Bar"]]]
           [:Directives [:Directive [:Name "foobar"]]]
           [:SelectionSet
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]
            [:Selection [:Field [:Name "c"]]]]]]]]))

(ns graphql-fmt.core-test
  (:require [clojure.test :refer [are deftest is testing]]
            [graphql-fmt.core :refer [document-parser
                                      ignored-parser
                                      token-parser]]))

(deftest test-ignored
  (are [input ast]
       (= ast (ignored-parser input))

    (str \uFEFF)
    []

    (str \u0009)
    []

    (str \u0020)
    []

    "\n"
    []

    "\r"
    []

    "\r\n"
    []

    "#"
    [[:Comment]]

    "# frobnitz"
    [[:Comment
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
    []))

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
  (is (= [:Token [:StringValue [:Quote] [:Quote]]] (token-parser "\"\"")))
  (is (= [:Token [:StringValue [:Quote] [:StringCharacter "*"] [:Quote]]]
         (token-parser "\"*\"")))
  (is (= [:Token
          [:StringValue
           [:Quote]
           [:StringCharacter "f"]
           [:StringCharacter "r"]
           [:StringCharacter "o"]
           [:StringCharacter "b"]
           [:Quote]]]
         (token-parser "\"frob\"")))
  (is (= [:Token
          [:StringValue
           [:Quote]
           [:StringCharacter "\\u" [:EscapedUnicode "0000"]]
           [:Quote]]]
         (token-parser "\"\\u0000\"")))
  (is (= [:Token
          [:StringValue
           [:Quote]
           [:StringCharacter "\\" [:EscapedCharacter "r"]]
           [:StringCharacter "\\" [:EscapedCharacter "n"]]
           [:Quote]]]
         (token-parser "\"\\r\\n\"")))
  (is (= [:Token
          [:StringValue
           [:BlockQuote]
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
           [:BlockStringCharacter " "]
           [:BlockQuote]]]
         (token-parser "\"\"\" frob\"frobnitz\"\"frobnicate \"\"\"")))
  (is (instance? instaparse.gll.Failure
                 (token-parser "\"\"\" \"\"\" \"\"\"")))
  (is (= [:Token
          [:StringValue
           [:BlockQuote]
           [:BlockStringCharacter " "]
           [:BlockStringCharacter "\"\"\""]
           [:BlockStringCharacter " "]
           [:BlockQuote]]]
         (token-parser "\"\"\" \\\"\"\" \"\"\"")))
  (is (= [:Token [:BooleanValue "true"]]
        (token-parser "true")))
  (is (= [:Token [:BooleanValue "false"]]
         (token-parser "false")))
  (is (= [:Token [:NullValue]]
         (token-parser "null"))))

(deftest test-document
  (are [document ast]
       (if (vector? document)
         (do
           (doseq [document-representation document]
             (is (= ast (document-parser document-representation))))
           true)
         (is (= ast (document-parser document))))

    ["{foo}"
     "{ foo }"
     " { foo } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition [:SelectionSet [:Selection [:Field [:Name "foo"]]]]]]]]

    ["{foo_alias:foo}"
     "{ foo_alias: foo }"
     "{ foo_alias : foo }"
     " { foo_alias : foo } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection [:Field [:Alias [:Name "foo_alias"]] [:Name "foo"]]]]]]]]

    ["{foo(bar:$foobar)}"
     "{ foo(bar: $foobar) }"
     "{ foo(bar : $ foobar) }"
     "{ foo ( bar : $ foobar ) }"
     " { foo ( bar : $ foobar ) } "]
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

    ["{foo(bar:1)}"
     "{ foo(bar: 1) }"
     "{ foo ( bar : 1 ) }"
     " { foo ( bar : 1 ) } "]
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

    ["{foo(bar:1.0)}"
     "{ foo(bar: 1.0) }"
     " { foo ( bar : 1.0 ) } "]
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

    ["{foo(bar:\"foobar\")}"
     "{ foo(bar: \"foobar\") }"
     " { foo ( bar : \"foobar\" ) } "]
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
               [:Quote]
               [:StringCharacter "f"]
               [:StringCharacter "o"]
               [:StringCharacter "o"]
               [:StringCharacter "b"]
               [:StringCharacter "a"]
               [:StringCharacter "r"]
               [:Quote]]]]]]]]]]]]

    ["{foo(bar:\"\"\"foobar\"\"\")}"
     "{ foo(bar: \"\"\"foobar\"\"\") }"
     " { foo ( bar : \"\"\"foobar\"\"\" ) } "]
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
               [:BlockQuote]
               [:BlockStringCharacter "f"]
               [:BlockStringCharacter "o"]
               [:BlockStringCharacter "o"]
               [:BlockStringCharacter "b"]
               [:BlockStringCharacter "a"]
               [:BlockStringCharacter "r"]
               [:BlockQuote]]]]]]]]]]]]

    ["{foo(bar:true)}"
     "{ foo(bar: true) }"
     " { foo ( bar : true ) }"]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments [:Argument [:Name "bar"] [:Value [:BooleanValue "true"]]]]]]]]]]]

    ["{foo(bar:null)}"
     "{ foo(bar: null) }"
     " { foo ( bar : null ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments [:Argument [:Name "bar"] [:Value [:NullValue]]]]]]]]]]]

    ["{foo(bar:[])}"
     "{ foo(bar: []) }"
     " { foo ( bar : [ ] ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments [:Argument [:Name "bar"] [:Value [:ListValue]]]]]]]]]]]

    ["{foo(bar:[1[1 2 3]$foobar])}"
     "{ foo(bar: [1 [1 2 3] $foobar]) }"
     " { foo ( bar : [ 1 [ 1 2 3 ] $ foobar ] ) } "]
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
              [:ListValue
               [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]
               [:Value
                [:ListValue
                 [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]
                 [:Value [:IntValue [:IntegerPart [:NonZeroDigit "2"]]]]
                 [:Value [:IntValue [:IntegerPart [:NonZeroDigit "3"]]]]]]
               [:Value [:Variable [:Name "foobar"]]]]]]]]]]]]]]

    ["{foo(bar:{})}"
     "{ foo(bar: {}) }"
     " { foo ( bar : { } ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments [:Argument [:Name "bar"] [:Value [:ObjectValue]]]]]]]]]]]

    ["{foo(bar:{foobar:1})}"
     "{ foo(bar: {foobar:1}) }"
     " { foo ( bar : { foobar : 1 } ) } "]
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
              [:ObjectValue
               [:ObjectField
                [:Name "foobar"]
                [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]]]]]]]]]]]]

    ["{foo(bar:{qux:1,baz:2})}"
     "{ foo(bar: { qux: 1, baz: 2 }) }"
     " { foo ( bar : { qux : 1 , baz : 2 } ) } "]
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
              [:ObjectValue
               [:ObjectField
                [:Name "qux"]
                [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]
               [:ObjectField
                [:Name "baz"]
                [:Value [:IntValue [:IntegerPart [:NonZeroDigit "2"]]]]]]]]]]]]]]]]

    ["{frob(foo:true,bar:false)}"
     "{ frob(foo: true, bar: false) }"
     " { frob ( foo : true , bar: false ) } "
     " { frob ( foo : true bar: false ) } "]
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

    ["{frob@foo}"
     "{frob @foo}"
     "{ frob @foo }"
     " { frob @ foo } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection
          [:Field [:Name "frob"] [:Directives [:Directive [:Name "foo"]]]]]]]]]]

    ["{frob@foo@bar}"
     "{ frob @foo @bar }"
     " { frob @ foo @ bar } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection
          [:Field
           [:Name "frob"]
           [:Directives [:Directive [:Name "foo"]] [:Directive [:Name "bar"]]]]]]]]]]

    ["{frob@foo(bar:true)}"
     "{ frob @foo(bar: true) }"
     " { frob @ foo ( bar : true ) } "]
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

    ["{frob@foo(a:1,b:2)}"
     "{ frob @foo(a:1, b:2) }"
     " { frob @foo ( a : 1 , b : 2 ) } "
     " { frob @ foo ( a : 1 b : 2 ) } "]
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

    ["{frob@foo(a:1)@bar(a:1)}"
     "{ frob @foo(a: 1) @bar(a: 1) }"
     " { frob @ foo ( a : 1 ) @ bar ( a : 1 ) } "]
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

    ["{foo{bar}}"
     "{ foo { bar } }"
     " { foo { bar } } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection
          [:Field [:Name "foo"] [:SelectionSet [:Selection [:Field [:Name "bar"]]]]]]]]]]]

    ["{foo{bar{foobar}}}"
     "{ foo { bar { foobar } } }"
     " { foo { bar { foobar } } } "]
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

    ;; TODO: Handle foo,bar in compact case
    ["{foo bar}"
     "{ foo bar }"
     " { foo bar } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection [:Field [:Name "foo"]]]
         [:Selection [:Field [:Name "bar"]]]]]]]]

    ["{foo{a}bar{a b}foobar{a{b}}}"
     "{ foo { a } bar { a b } foobar { a { b } } }"
     " { foo { a } bar { a b } foobar { a { b } } } "]
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

    ["{...frob}"
     "{ ...frob }"
     " { ... frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet [:Selection [:FragmentSpread [:FragmentName "frob"]]]]]]]]

    ["{...frob@foo}"
     "{ ...frob @foo }"
     " { ... frob @ foo } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection
          [:FragmentSpread
           [:FragmentName "frob"]
           [:Directives [:Directive [:Name "foo"]]]]]]]]]]

    ["{...foo...bar}"
     "{ ...foo ...bar }"
     " { ... foo ... bar } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection [:FragmentSpread [:FragmentName "foo"]]]
         [:Selection [:FragmentSpread [:FragmentName "bar"]]]]]]]]

    ["{...{frob}}"
     "{ ... { frob } }"
     " { ... { frob } } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection
          [:InlineFragment [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]]]]

    ["{...on Foo{...on Bar{foobar}}}"
     "{ ... on Foo { ... on Bar { foobar } } }"
     " { ... on Foo { ... on Bar { foobar } } } "]
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

    ["{...on Foo@bar{foobar}}"
     "{ ... on Foo @bar { foobar } }"
     " { ... on Foo @ bar { foobar } } "]
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

    ["query{frob}"
     "query { frob }"
     " query { frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]

    ["mutation{frob}"
     "mutation { frob }"
     " mutation { frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "mutation"]
        [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]

    ["subscription{frob}"
     "subscription { frob }"
     " subscription { frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "subscription"]
        [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]

    ["query frobnicator{frob}"
     "query frobnicator { frob }"
     " query frobnicator { frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frobnicator"]
        [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]

    ["query frobnicator@foo{frob}"
     "query frobnicator @foo { frob }"
     " query frobnicator @foo { frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frobnicator"]
        [:Directives [:Directive [:Name "foo"]]]
        [:SelectionSet [:Selection [:Field [:Name "frob"]]]]]]]]

    ["query frob($foo:bar){a b}"
     "query frob($foo: bar) { a b }"
     " query frob ( $ foo : bar ) { a b } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frob"]
        [:VariableDefinitions
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:NamedType [:Name "bar"]]]]]
        [:SelectionSet
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]]]]]]

    ["query frob($foo:bar$qux:baz){a b}"
     "query frob($foo: bar $qux: baz) { a b }"
     " query frob ( $ foo : bar $ qux : baz ) { a b } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frob"]
        [:VariableDefinitions
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:NamedType [:Name "bar"]]]]
         [:VariableDefinition
          [:Variable [:Name "qux"]]
          [:Colon ":"]
          [:Type [:NamedType [:Name "baz"]]]]]
        [:SelectionSet
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]]]]]]

    ["query frob($foo:[bar]){a b}"
     "query frob($foo: [bar]) { a b }"
     " query frob ( $ foo : [ bar ] ) { a b } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frob"]
        [:VariableDefinitions
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:ListType
                  [:BracketOpen "["]
                  [:Type [:NamedType [:Name "bar"]]]
                  [:BracketClose "]"]]]]]
        [:SelectionSet
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]]]]]]

    ["query frob($foo:[[bar]]){a b}"
     "query frob($foo: [[bar]]) { a b }"
     " query frob ( $ foo : [ [ bar ] ] ) { a b } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frob"]
        [:VariableDefinitions
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:ListType
                  [:BracketOpen "["]
                  [:Type [:ListType
                          [:BracketOpen "["]
                          [:Type [:NamedType [:Name "bar"]]]
                          [:BracketClose "]"]]]
                  [:BracketClose "]"]]]]]
        [:SelectionSet
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]]]]]]

    ["query frob($foo:bar!){a b}"
     "query frob($foo: bar!) { a b }"
     " query frob ( $ foo : bar ! ) { a b } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frob"]
        [:VariableDefinitions
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:NonNullType [:NamedType [:Name "bar"]] [:ExclamationMark "!"]]]]]
        [:SelectionSet
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]]]]]]

    ["query frob($foo:[bar]!){a b}"
     "query frob($foo: [bar]!) { a b }"
     " query frob ( $ foo : [ bar ] ! ) { a b } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frob"]
        [:VariableDefinitions
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:NonNullType
                  [:ListType
                   [:BracketOpen "["]
                   [:Type [:NamedType [:Name "bar"]]]
                   [:BracketClose "]"]]
                  [:ExclamationMark "!"]]]]]
        [:SelectionSet
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]]]]]]

    ["query frob($foo:[bar!]){a b}"
     "query frob($foo: [bar!]) { a b }"
     " query frob ( $ foo : [ bar ! ] ) { a b } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frob"]
        [:VariableDefinitions
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:ListType
                  [:BracketOpen "["]
                  [:Type [:NonNullType
                          [:NamedType [:Name "bar"]]
                          [:ExclamationMark "!"]]]
                  [:BracketClose "]"]]]]]
        [:SelectionSet
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]]]]]]

    ["query frob($foo:[bar!]!){a b}"
     "query frob($foo: [bar!]!) { a b }"
     " query frob ( $ foo : [ bar ! ] ! ) { a b } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frob"]
        [:VariableDefinitions
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type
           [:NonNullType
            [:ListType
             [:BracketOpen "["]
             [:Type [:NonNullType
                     [:NamedType [:Name "bar"]]
                     [:ExclamationMark "!"]]]
             [:BracketClose "]"]]
            [:ExclamationMark "!"]]]]]
        [:SelectionSet
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]]]]]]

    ["query frob($foo:bar=true){a b}"
     "query frob($foo: bar = true) { a b }"
     " query frob ( $ foo : bar = true ) { a b } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frob"]
        [:VariableDefinitions
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:NamedType [:Name "bar"]]]
          [:DefaultValue
           [:Equals "="]
           [:Value [:BooleanValue "true"]]]]]
        [:SelectionSet
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]]]]]]

    ["fragment foo on Bar@foobar{a b c}"
     "fragment foo on Bar @foobar { a b c }"
     " fragment foo on Bar @ foobar { a b c } "]
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
         [:Selection [:Field [:Name "c"]]]]]]]]

    ["schema{query:Foo subscription:Bar mutation:Foobar}"
     "schema { query: Foo subscription: Bar mutation: Foobar }"
     " schema { query : Foo subscription : Bar mutation : Foobar } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:SchemaDefinition
        [:RootOperationTypeDefinition
         [:OperationType "query"]
         [:Colon ":"]
         [:NamedType [:Name "Foo"]]]
        [:RootOperationTypeDefinition
         [:OperationType "subscription"]
         [:Colon ":"]
         [:NamedType [:Name "Bar"]]]
        [:RootOperationTypeDefinition
         [:OperationType "mutation"]
         [:Colon ":"]
         [:NamedType [:Name "Foobar"]]]]]]]

    ["schema@foo{query:Foo}"
     "schema @foo { query: Foo }"
     " schema @ foo { query : Foo } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:SchemaDefinition
        [:Directives [:Directive [:Name "foo"]]]
        [:RootOperationTypeDefinition
         [:OperationType "query"]
         [:Colon ":"]
         [:NamedType [:Name "Foo"]]]]]]]

    ["\"the scalar\"scalar Foo@bar"
     "\"the scalar\" scalar Foo @bar"
     " \"the scalar\" scalar Foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ScalarTypeDefinition
         [:Description
          [:StringValue
           [:Quote]
           [:StringCharacter "t"]
           [:StringCharacter "h"]
           [:StringCharacter "e"]
           [:StringCharacter " "]
           [:StringCharacter "s"]
           [:StringCharacter "c"]
           [:StringCharacter "a"]
           [:StringCharacter "l"]
           [:StringCharacter "a"]
           [:StringCharacter "r"]
           [:Quote]]]
         [:Name "Foo"]
         [:Directives [:Directive [:Name "bar"]]]]]]]]

    ["\"documents the\"type Foo@bar"
     "\"documents the\" type Foo @bar"
     " \"documents the\" type Foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:Description
          [:StringValue
           [:Quote]
           [:StringCharacter "d"]
           [:StringCharacter "o"]
           [:StringCharacter "c"]
           [:StringCharacter "u"]
           [:StringCharacter "m"]
           [:StringCharacter "e"]
           [:StringCharacter "n"]
           [:StringCharacter "t"]
           [:StringCharacter "s"]
           [:StringCharacter " "]
           [:StringCharacter "t"]
           [:StringCharacter "h"]
           [:StringCharacter "e"]
           [:Quote]]]
         [:Name "Foo"]
         [:Directives [:Directive [:Name "bar"]]]]]]]]

    ["type Foo{\"the field definition\"Bar:String@foobar}"
     "type Foo{ \"the field definition\" Bar: String @foobar }"
     " type Foo { \"the field definition\" Bar : String @ foobar } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:Name "Foo"]
         [:FieldsDefinition
          [:FieldDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacter "t"]
             [:StringCharacter "h"]
             [:StringCharacter "e"]
             [:StringCharacter " "]
             [:StringCharacter "f"]
             [:StringCharacter "i"]
             [:StringCharacter "e"]
             [:StringCharacter "l"]
             [:StringCharacter "d"]
             [:StringCharacter " "]
             [:StringCharacter "d"]
             [:StringCharacter "e"]
             [:StringCharacter "f"]
             [:StringCharacter "i"]
             [:StringCharacter "n"]
             [:StringCharacter "i"]
             [:StringCharacter "t"]
             [:StringCharacter "i"]
             [:StringCharacter "o"]
             [:StringCharacter "n"]
             [:Quote]]]
           [:Name "Bar"]
           [:Type [:NamedType [:Name "String"]]]
           [:Directives [:Directive [:Name "foobar"]]]]]]]]]]

    ["type Foo{Bar:String}"
     "type Foo { Bar: String }"
     " type Foo { Bar : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:Name "Foo"]
         [:FieldsDefinition
          [:FieldDefinition [:Name "Bar"] [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["type Foo{Qux:String Baz:String}"
     "type Foo { Qux: String Baz: String }"
     " type Foo { Qux : String Baz : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:Name "Foo"]
         [:FieldsDefinition
          [:FieldDefinition [:Name "Qux"] [:Type [:NamedType [:Name "String"]]]]
          [:FieldDefinition [:Name "Baz"] [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["type Foo implements Bar{qux:String}"
     "type Foo implements Bar { qux: String }"
     " type Foo implements Bar { qux : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:Name "Foo"]
         [:ImplementsInterfaces [:NamedType [:Name "Bar"]]]
         [:FieldsDefinition
          [:FieldDefinition [:Name "qux"] [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["type Foo implements&Bar{qux:String}"
     "type Foo implements & Bar { qux: String }"
     " type Foo implements & Bar { qux : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:Name "Foo"]
         [:ImplementsInterfaces [:NamedType [:Name "Bar"]]]
         [:FieldsDefinition
          [:FieldDefinition [:Name "qux"] [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["type Foo implements Bar&Foobar{qux:String}"
     "type Foo implements Bar & Foobar { qux: String }"
     " type Foo implements Bar & Foobar { qux : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:Name "Foo"]
         [:ImplementsInterfaces
          [:ImplementsInterfaces [:NamedType [:Name "Bar"]]]
          [:NamedType [:Name "Foobar"]]]
         [:FieldsDefinition
          [:FieldDefinition [:Name "qux"] [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["interface Foo{qux:String}"
     "interface Foo { qux: String }"
     " interface Foo { qux : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:InterfaceTypeDefinition
         [:Name "Foo"]
         [:FieldsDefinition
          [:FieldDefinition [:Name "qux"] [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["\"the\"interface Foo@bar{\"the\"qux:String\"the\"baz:String}"
     "\"the\" interface Foo @bar { \"the\" qux: String \"the\" baz: String }"
     " \"the\" interface Foo @ bar { \"the\" qux : String \"the\" baz : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:InterfaceTypeDefinition
         [:Description
          [:StringValue
           [:Quote]
           [:StringCharacter "t"]
           [:StringCharacter "h"]
           [:StringCharacter "e"]
           [:Quote]]]
         [:Name "Foo"]
         [:Directives [:Directive [:Name "bar"]]]
         [:FieldsDefinition
          [:FieldDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacter "t"]
             [:StringCharacter "h"]
             [:StringCharacter "e"]
             [:Quote]]]
           [:Name "qux"]
           [:Type [:NamedType [:Name "String"]]]]
          [:FieldDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacter "t"]
             [:StringCharacter "h"]
             [:StringCharacter "e"]
             [:Quote]]]
           [:Name "baz"]
           [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["union Foobar"
     " union Foobar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition [:UnionTypeDefinition [:Name "Foobar"]]]]]]

    ["union Foo=Bar"
     " union Foo = Bar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:UnionTypeDefinition
         [:Name "Foo"]
         [:UnionMemberTypes [:NamedType [:Name "Bar"]]]]]]]]

    ["union Foobar=Foo|Bar"
     " union Foobar = Foo | Bar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:UnionTypeDefinition
         [:Name "Foobar"]
         [:UnionMemberTypes
          [:UnionMemberTypes [:NamedType [:Name "Foo"]]]
          [:NamedType [:Name "Bar"]]]]]]]]

    ["\"the\"union Foobar@qux=Foo|Bar"
     "\"the\" union Foobar @qux = Foo | Bar"
     " \"the\" union Foobar @ qux = Foo | Bar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:UnionTypeDefinition
         [:Description
          [:StringValue
           [:Quote]
           [:StringCharacter "t"]
           [:StringCharacter "h"]
           [:StringCharacter "e"]
           [:Quote]]]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:UnionMemberTypes
          [:UnionMemberTypes [:NamedType [:Name "Foo"]]]
          [:NamedType [:Name "Bar"]]]]]]]]

    ["enum Foobar"
     " enum Foobar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition [:EnumTypeDefinition [:Name "Foobar"]]]]]]

    ["enum Foobar{FOO}"
     " enum Foobar { FOO } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:EnumTypeDefinition
         [:Name "Foobar"]
         [:EnumValuesDefinition [:EnumValueDefinition [:EnumValue "FOO"]]]]]]]]

    ["enum Foobar{FOO BAR}"
     " enum Foobar { FOO BAR } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:EnumTypeDefinition
         [:Name "Foobar"]
         [:EnumValuesDefinition
          [:EnumValueDefinition [:EnumValue "FOO"]]
          [:EnumValueDefinition [:EnumValue "BAR"]]]]]]]]

    ["\"the\"enum Foobar@qux{\"it foo\"FOO\"it bar\"BAR}"
     "\"the\" enum Foobar @qux { \"it foo\" FOO \"it bar\" BAR }"
     " \"the\" enum Foobar @ qux { \"it foo\" FOO \"it bar\" BAR } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:EnumTypeDefinition
         [:Description
          [:StringValue
           [:Quote]
           [:StringCharacter "t"]
           [:StringCharacter "h"]
           [:StringCharacter "e"]
           [:Quote]]]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:EnumValuesDefinition
          [:EnumValueDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacter "i"]
             [:StringCharacter "t"]
             [:StringCharacter " "]
             [:StringCharacter "f"]
             [:StringCharacter "o"]
             [:StringCharacter "o"]
             [:Quote]]]
           [:EnumValue "FOO"]]
          [:EnumValueDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacter "i"]
             [:StringCharacter "t"]
             [:StringCharacter " "]
             [:StringCharacter "b"]
             [:StringCharacter "a"]
             [:StringCharacter "r"]
             [:Quote]]]
           [:EnumValue "BAR"]]]]]]]]

    ["input Foobar"
     " input Foobar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition [:InputObjectTypeDefinition [:Name "Foobar"]]]]]]

    ["input Foobar{foo:String}"
     "input Foobar { foo: String }"
     " input Foobar { foo : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:InputObjectTypeDefinition
         [:Name "Foobar"]
         [:InputFieldsDefinition
          [:InputValueDefinition
           [:Name "foo"]
           [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["input Foobar{foo:String bar:String}"
     "input Foobar { foo: String bar: String}"
     " input Foobar { foo : String bar : String} "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:InputObjectTypeDefinition
         [:Name "Foobar"]
         [:InputFieldsDefinition
          [:InputValueDefinition
           [:Name "foo"]
           [:Type [:NamedType [:Name "String"]]]]
          [:InputValueDefinition
           [:Name "bar"]
           [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["\"the\"input Foobar@qux{\"the\"foo:String=\"foo\"@qux\"the\"bar:String=\"bar\"@qux}"
     "\"the\" input Foobar @qux { \"the\" foo: String = \"foo\" @qux \"the\" bar: String = \"bar\" @qux }"
     " \"the\" input Foobar @ qux { \"the\" foo : String = \"foo\" @ qux \"the\" bar : String = \"bar\" @ qux } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:InputObjectTypeDefinition
         [:Description
          [:StringValue
           [:Quote]
           [:StringCharacter "t"]
           [:StringCharacter "h"]
           [:StringCharacter "e"]
           [:Quote]]]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:InputFieldsDefinition
          [:InputValueDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacter "t"]
             [:StringCharacter "h"]
             [:StringCharacter "e"]
             [:Quote]]]
           [:Name "foo"]
           [:Type [:NamedType [:Name "String"]]]
           [:DefaultValue
            [:Equals "="]
            [:Value
             [:StringValue
              [:Quote]
              [:StringCharacter "f"]
              [:StringCharacter "o"]
              [:StringCharacter "o"]
              [:Quote]]]]
           [:Directives [:Directive [:Name "qux"]]]]
          [:InputValueDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacter "t"]
             [:StringCharacter "h"]
             [:StringCharacter "e"]
             [:Quote]]]
           [:Name "bar"]
           [:Type [:NamedType [:Name "String"]]]
           [:DefaultValue
            [:Equals "="]
            [:Value
             [:StringValue
              [:Quote]
              [:StringCharacter "b"]
              [:StringCharacter "a"]
              [:StringCharacter "r"]
              [:Quote]]]]
           [:Directives [:Directive [:Name "qux"]]]]]]]]]]

    ["directive@foo on FIELD"
     "directive @foo on FIELD"
     " directive @ foo on FIELD "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:DirectiveDefinition
        [:Name "foo"]
        [:DirectiveLocations
         [:DirectiveLocation [:ExecutableDirectiveLocation "FIELD"]]]]]]]

    ["directive@foo on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive@foo on|FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive @foo on | FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT"
     " directive @ foo on | FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:DirectiveDefinition
        [:Name "foo"]
        [:DirectiveLocations
         [:DirectiveLocations
          [:DirectiveLocations
           [:DirectiveLocation [:ExecutableDirectiveLocation "FIELD"]]]
          [:DirectiveLocation [:ExecutableDirectiveLocation "FRAGMENT_SPREAD"]]]
         [:DirectiveLocation [:ExecutableDirectiveLocation "INLINE_FRAGMENT"]]]]]]]

    ["directive@foo(qux:String baz:String)on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive@foo(qux:String baz:String)on|FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive @foo(qux: String baz: String) on | FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT"
     " directive @ foo ( qux : String baz : String ) on | FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:DirectiveDefinition
        [:Name "foo"]
        [:ArgumentsDefinition
         [:InputValueDefinition [:Name "qux"] [:Type [:NamedType [:Name "String"]]]]
         [:InputValueDefinition [:Name "baz"] [:Type [:NamedType [:Name "String"]]]]]
        [:DirectiveLocations
         [:DirectiveLocations
          [:DirectiveLocations
           [:DirectiveLocation [:ExecutableDirectiveLocation "FIELD"]]]
          [:DirectiveLocation [:ExecutableDirectiveLocation "FRAGMENT_SPREAD"]]]
         [:DirectiveLocation [:ExecutableDirectiveLocation "INLINE_FRAGMENT"]]]]]]]

    ["extend schema@foo@bar"
     "extend schema @foo @bar"
     " extend schema @ foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:SchemaExtension
        [:Directives [:Directive [:Name "foo"]] [:Directive [:Name "bar"]]]]]]]

    ["extend schema{query:frobnicate}"
     "extend schema { query: frobnicate }"
     " extend schema { query : frobnicate } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:SchemaExtension
        [:OperationTypeDefinition
         [:OperationType "query"]
         [:NamedType [:Name "frobnicate"]]]]]]]

    ["extend schema@foo{query:frobnicate mutation:frobnitz}"
     "extend schema @foo { query: frobnicate mutation: frobnitz }"
     " extend schema @ foo { query : frobnicate mutation : frobnitz } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:SchemaExtension
        [:Directives [:Directive [:Name "foo"]]]
        [:OperationTypeDefinition
         [:OperationType "query"]
         [:NamedType [:Name "frobnicate"]]]
        [:OperationTypeDefinition
         [:OperationType "mutation"]
         [:NamedType [:Name "frobnitz"]]]]]]]

    ["extend scalar Foo@bar"
     "extend scalar Foo @bar"
     " extend scalar Foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ScalarTypeExtension
         [:Name "Foo"]
         [:Directives [:Directive [:Name "bar"]]]]]]]]

    "extend type Foo implements Qux"
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ObjectTypeExtension
         [:Name "Foo"]
         [:ImplementsInterfaces [:NamedType [:Name "Qux"]]]]]]]]

    ["extend type Foo implements Qux&Baz"
     " extend type Foo implements Qux & Baz "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ObjectTypeExtension
         [:Name "Foo"]
         [:ImplementsInterfaces
          [:ImplementsInterfaces [:NamedType [:Name "Qux"]]]
          [:NamedType [:Name "Baz"]]]]]]]]

    ["extend type Foo@bar"
     "extend type Foo @bar"
     " extend type Foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ObjectTypeExtension
         [:Name "Foo"]
         [:Directives [:Directive [:Name "bar"]]]]]]]]

    ["extend type Foo implements Qux&Baz@bar"
     "extend type Foo implements Qux & Baz @bar"
     " extend type Foo implements Qux & Baz @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ObjectTypeExtension
         [:Name "Foo"]
         [:ImplementsInterfaces
          [:ImplementsInterfaces [:NamedType [:Name "Qux"]]]
          [:NamedType [:Name "Baz"]]]
         [:Directives [:Directive [:Name "bar"]]]]]]]]

    ["extend type Foo{qux:String}"
     "extend type Foo { qux: String }"
     " extend type Foo { qux : String } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ObjectTypeExtension
         [:Name "Foo"]
         [:FieldsDefinition
          [:FieldDefinition [:Name "qux"] [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["extend type Foo implements Bar@foobar{\"the\"qux:String@baz}"
     "extend type Foo implements Bar @foobar { \"the\" qux: String @baz }"
     " extend type Foo implements Bar @ foobar { \"the\" qux : String @ baz } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ObjectTypeExtension
         [:Name "Foo"]
         [:ImplementsInterfaces [:NamedType [:Name "Bar"]]]
         [:Directives [:Directive [:Name "foobar"]]]
         [:FieldsDefinition
          [:FieldDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacter "t"]
             [:StringCharacter "h"]
             [:StringCharacter "e"]
             [:Quote]]]
           [:Name "qux"]
           [:Type [:NamedType [:Name "String"]]]
           [:Directives [:Directive [:Name "baz"]]]]]]]]]]

    ["extend interface Foo@bar"
     "extend interface Foo @bar"
     " extend interface Foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:InterfaceTypeExtension
         [:Name "Foo"]
         [:Directives [:Directive [:Name "bar"]]]]]]]]

    ["extend interface Foobar{foo:String bar:String}"
     "extend interface Foobar { foo: String bar: String }"
     " extend interface Foobar { foo : String bar : String } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:InterfaceTypeExtension
         [:Name "Foobar"]
         [:FieldsDefinition
          [:FieldDefinition [:Name "foo"] [:Type [:NamedType [:Name "String"]]]]
          [:FieldDefinition [:Name "bar"] [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["extend interface Foobar@foo{bar:String}"
     "extend interface Foobar @foo { bar: String }"
     " extend interface Foobar @ foo { bar : String } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:InterfaceTypeExtension
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "foo"]]]
         [:FieldsDefinition
          [:FieldDefinition [:Name "bar"] [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["extend union Foobar@foo@bar"
     "extend union Foobar @foo @bar"
     " extend union Foobar @ foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:UnionTypeExtension
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "foo"]] [:Directive [:Name "bar"]]]]]]]]

    ["extend union Foobar=Qux|Baz"
     " extend union Foobar = Qux | Baz "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:UnionTypeExtension
         [:Name "Foobar"]
         [:UnionMemberTypes
          [:UnionMemberTypes [:NamedType [:Name "Qux"]]]
          [:NamedType [:Name "Baz"]]]]]]]]

    ["extend union Foobar@qux=Baz"
     "extend union Foobar @qux = Baz"
     " extend union Foobar @ qux = Baz "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:UnionTypeExtension
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:UnionMemberTypes [:NamedType [:Name "Baz"]]]]]]]]

    ["extend enum Foobar@foo@bar"
     "extend enum Foobar @foo @bar"
     " extend enum Foobar @ foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:EnumTypeExtension
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "foo"]] [:Directive [:Name "bar"]]]]]]]]

    ["extend enum Foobar{QUX BAZ}"
     " extend enum Foobar { QUX BAZ } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:EnumTypeExtension
         [:Name "Foobar"]
         [:EnumValuesDefinition
          [:EnumValueDefinition [:EnumValue "QUX"]]
          [:EnumValueDefinition [:EnumValue "BAZ"]]]]]]]]

    ["extend enum Foobar@qux{BAZ}"
     "extend enum Foobar @qux { BAZ }"
     " extend enum Foobar @ qux { BAZ } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:EnumTypeExtension
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:EnumValuesDefinition [:EnumValueDefinition [:EnumValue "BAZ"]]]]]]]]

    ["extend input Foobar@foo@bar"
     "extend input Foobar @foo @bar"
     " extend input Foobar @ foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:InputObjectTypeExtension
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "foo"]] [:Directive [:Name "bar"]]]]]]]]

    ["extend input Foobar{qux:String baz:String}"
     "extend input Foobar { qux: String baz: String }"
     "extend input Foobar { qux : String baz : String } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:InputObjectTypeExtension
         [:Name "Foobar"]
         [:InputFieldsDefinition
          [:InputValueDefinition
           [:Name "qux"]
           [:Type [:NamedType [:Name "String"]]]]
          [:InputValueDefinition
           [:Name "baz"]
           [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["extend input Foobar@qux{baz:String}"
     "extend input Foobar @qux { baz: String }"
     " extend input Foobar @ qux { baz : String } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:InputObjectTypeExtension
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:InputFieldsDefinition
          [:InputValueDefinition
           [:Name "baz"]
           [:Type [:NamedType [:Name "String"]]]]]]]]]]

    ["query{foo}{foo:String}fragment foo on Bar{foo}type Foo schema{query:Foo}"
     "query {foo} { foo : String } fragment foo on Bar {foo} type Foo schema {query:Foo}"
     " query { foo } { foo : String } fragment foo on Bar { foo } type Foo schema { query : Foo } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:SelectionSet [:Selection [:Field [:Name "foo"]]]]]]]
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:Selection [:Field [:Alias [:Name "foo"]] [:Name "String"]]]]]]]
     [:Definition
      [:ExecutableDefinition
       [:FragmentDefinition
        [:FragmentName "foo"]
        [:TypeCondition [:NamedType [:Name "Bar"]]]
        [:SelectionSet [:Selection [:Field [:Name "foo"]]]]]]]
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition [:ObjectTypeDefinition [:Name "Foo"]]]]]
     [:Definition
      [:TypeSystemDefinition
       [:SchemaDefinition
        [:RootOperationTypeDefinition
         [:OperationType "query"]
         [:Colon ":"]
         [:NamedType [:Name "Foo"]]]]]]]))

(ns graphqlfmt.core-test
  (:refer-clojure :exclude [name comment])
  (:require [clojure.test :refer [are deftest is]]
            [graphqlfmt.ast :as ast]
            [graphqlfmt.core :refer :all]
            [instaparse.core :as insta]))

(def ignored-parser
  (insta/parser (ast/ebnf "ignored")))

(def token-parser
  (insta/parser (ast/ebnf "token")))

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
      [:CommentChar "f"]
      [:CommentChar "r"]
      [:CommentChar "o"]
      [:CommentChar "b"]
      [:CommentChar "n"]
      [:CommentChar "i"]
      [:CommentChar "t"]
      [:CommentChar "z"]]]

    ","
    []

    "#       hello"
    [[:Comment
      [:CommentChar "h"]
      [:CommentChar "e"]
      [:CommentChar "l"]
      [:CommentChar "l"]
      [:CommentChar "o"]]]))

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
  (is (= [:Token [:StringValue [:Quote] [:StringCharacters] [:Quote]]] (token-parser "\"\"")))
  (is (= [:Token [:StringValue [:Quote] [:StringCharacters
                                         [:StringCharacter "*"]] [:Quote]]]
         (token-parser "\"*\"")))
  (is (= [:Token
          [:StringValue
           [:Quote]
           [:StringCharacters
            [:StringCharacter "f"]
            [:StringCharacter "r"]
            [:StringCharacter "o"]
            [:StringCharacter "b"]]
           [:Quote]]]
         (token-parser "\"frob\"")))
  (is (= [:Token
          [:StringValue
           [:Quote]
           [:StringCharacters
            [:StringCharacter "\\u"
             [:EscapedUnicode "0000"]]]
           [:Quote]]]
         (token-parser "\"\\u0000\"")))
  (is (= [:Token
          [:StringValue
           [:Quote]
           [:StringCharacters
            [:StringCharacter "\\" [:EscapedCharacter "r"]]
            [:StringCharacter "\\" [:EscapedCharacter "n"]]]
           [:Quote]]]
         (token-parser "\"\\r\\n\"")))
  (is (= [:Token
          [:StringValue
           [:BlockQuoteOpen]
           [:BlockStringCharacters
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
            [:BlockStringCharacter " "]]
           [:BlockQuoteClose]]]
         (token-parser "\"\"\" frob\"frobnitz\"\"frobnicate \"\"\"")))
  (is (instance? instaparse.gll.Failure
                 (token-parser "\"\"\" \"\"\" \"\"\"")))
  (is (= [:Token
          [:StringValue
           [:BlockQuoteOpen]
           [:BlockStringCharacters
            [:BlockStringCharacter " "]
            [:BlockStringCharacter "\"\"\""]
            [:BlockStringCharacter " "]]
           [:BlockQuoteClose]]]
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
             (is (= ast (ast/document-parser document-representation)) document-representation))
           true)
         (is (= ast (ast/document-parser document)) document))

    ["{foo}"
     "{ foo }"
     " { foo } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "foo"]]]
         [:BraceClose "}"]]]]]]

    ["{foo_alias:foo}"
     "{ foo_alias: foo }"
     "{ foo_alias : foo }"
     " { foo_alias : foo } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field
                      [:Alias [:Name "foo_alias"] [:Colon ":"]]
                      [:Name "foo"]]]
         [:BraceClose "}"]]]]]]

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
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value [:Variable [:Name "foobar"]]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:1)}"
     "{ foo(bar: 1) }"
     "{ foo ( bar : 1 ) }"
     " { foo ( bar : 1 ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:1.0)}"
     "{ foo(bar: 1.0) }"
     " { foo ( bar : 1.0 ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value
              [:FloatValue
               [:IntegerPart [:NonZeroDigit "1"]]
               [:FractionalPart [:Digit "0"]]]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:\"foobar\")}"
     "{ foo(bar: \"foobar\") }"
     " { foo ( bar : \"foobar\" ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value
              [:StringValue
               [:Quote]
               [:StringCharacters
                [:StringCharacter "f"]
                [:StringCharacter "o"]
                [:StringCharacter "o"]
                [:StringCharacter "b"]
                [:StringCharacter "a"]
                [:StringCharacter "r"]]
               [:Quote]]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:\"\"\"foobar\"\"\")}"
     "{ foo(bar: \"\"\"foobar\"\"\") }"
     " { foo ( bar : \"\"\"foobar\"\"\" ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value
              [:StringValue
               [:BlockQuoteOpen]
               [:BlockStringCharacters
                [:BlockStringCharacter "f"]
                [:BlockStringCharacter "o"]
                [:BlockStringCharacter "o"]
                [:BlockStringCharacter "b"]
                [:BlockStringCharacter "a"]
                [:BlockStringCharacter "r"]]
               [:BlockQuoteClose]]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:true)}"
     "{ foo(bar: true) }"
     " { foo ( bar : true ) }"]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value [:BooleanValue "true"]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:null)}"
     "{ foo(bar: null) }"
     " { foo ( bar : null ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value [:NullValue]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:[])}"
     "{ foo(bar: []) }"
     " { foo ( bar : [ ] ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value [:ListValue]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:[1[1 2 3]$foobar])}"
     "{ foo(bar: [1 [1 2 3] $foobar]) }"
     " { foo ( bar : [ 1 [ 1 2 3 ] $ foobar ] ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value
              [:ListValue
               [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]
               [:Value
                [:ListValue
                 [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]
                 [:Value [:IntValue [:IntegerPart [:NonZeroDigit "2"]]]]
                 [:Value [:IntValue [:IntegerPart [:NonZeroDigit "3"]]]]]]
               [:Value [:Variable [:Name "foobar"]]]]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:{})}"
     "{ foo(bar: {}) }"
     " { foo ( bar : { } ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value [:ObjectValue [:BraceOpen "{"] [:BraceClose "}"]]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:{foobar:1})}"
     "{ foo(bar: {foobar:1}) }"
     " { foo ( bar : { foobar : 1 } ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value
              [:ObjectValue
               [:BraceOpen "{"]
               [:ObjectField
                [:Name "foobar"]
                [:Colon ":"]
                [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]
               [:BraceClose "}"]]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo(bar:{qux:1,baz:2})}"
     "{ foo(bar: { qux: 1, baz: 2 }) }"
     " { foo ( bar : { qux : 1 , baz : 2 } ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value
              [:ObjectValue
               [:BraceOpen "{"]
               [:ObjectField
                [:Name "qux"]
                [:Colon ":"]
                [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]
               [:ObjectField
                [:Name "baz"]
                [:Colon ":"]
                [:Value [:IntValue [:IntegerPart [:NonZeroDigit "2"]]]]]
               [:BraceClose "}"]]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{frob(foo:true,bar:false)}"
     "{ frob(foo: true, bar: false) }"
     " { frob ( foo : true , bar: false ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "frob"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "foo"]
             [:Colon ":"]
             [:Value [:BooleanValue "true"]]]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value [:BooleanValue "false"]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    [" { frob ( foo : true bar: false ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "frob"]
           [:Arguments
            [:ParensOpen "("]
            [:Argument
             [:Name "foo"]
             [:Colon ":"]
             [:Value [:BooleanValue "true"]]]
            [:Argument
             [:Name "bar"]
             [:Colon ":"]
             [:Value [:BooleanValue "false"]]]
            [:ParensClose ")"]]]]
         [:BraceClose "}"]]]]]]

    ["{frob@foo}"
     "{frob @foo}"
     "{ frob @foo }"
     " { frob @ foo } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field [:Name "frob"] [:Directives [:Directive [:Name "foo"]]]]]
         [:BraceClose "}"]]]]]]

    ["{frob@foo@bar}"
     "{ frob @foo @bar }"
     " { frob @ foo @ bar } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "frob"]
           [:Directives [:Directive [:Name "foo"]] [:Directive [:Name "bar"]]]]]
         [:BraceClose "}"]]]]]]

    ["{frob@foo(bar:true)}"
     "{ frob @foo(bar: true) }"
     " { frob @ foo ( bar : true ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "frob"]
           [:Directives
            [:Directive
             [:Name "foo"]
             [:Arguments
              [:ParensOpen "("]
              [:Argument
               [:Name "bar"]
               [:Colon ":"]
               [:Value [:BooleanValue "true"]]]
              [:ParensClose ")"]]]]]]
         [:BraceClose "}"]]]]]]

    ["{frob@foo(a:1,b:2)}"
     "{ frob @foo(a:1, b:2) }"
     " { frob @foo ( a : 1 , b : 2 ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "frob"]
           [:Directives
            [:Directive
             [:Name "foo"]
             [:Arguments
              [:ParensOpen "("]
              [:Argument
               [:Name "a"]
               [:Colon ":"]
               [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]
              [:Argument
               [:Name "b"]
               [:Colon ":"]
               [:Value [:IntValue [:IntegerPart [:NonZeroDigit "2"]]]]]
              [:ParensClose ")"]]]]]]
         [:BraceClose "}"]]]]]]

    [" { frob @ foo ( a : 1 b : 2 ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "frob"]
           [:Directives
            [:Directive
             [:Name "foo"]
             [:Arguments
              [:ParensOpen "("]
              [:Argument
               [:Name "a"]
               [:Colon ":"]
               [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]
              [:Argument
               [:Name "b"]
               [:Colon ":"]
               [:Value [:IntValue [:IntegerPart [:NonZeroDigit "2"]]]]]
              [:ParensClose ")"]]]]]]
         [:BraceClose "}"]]]]]]

    ["{frob@foo(a:1)@bar(a:1)}"
     "{ frob @foo(a: 1) @bar(a: 1) }"
     " { frob @ foo ( a : 1 ) @ bar ( a : 1 ) } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "frob"]
           [:Directives
            [:Directive
             [:Name "foo"]
             [:Arguments
              [:ParensOpen "("]
              [:Argument
               [:Name "a"]
               [:Colon ":"]
               [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]
              [:ParensClose ")"]]]
            [:Directive
             [:Name "bar"]
             [:Arguments
              [:ParensOpen "("]
              [:Argument
               [:Name "a"]
               [:Colon ":"]
               [:Value [:IntValue [:IntegerPart [:NonZeroDigit "1"]]]]]
              [:ParensClose ")"]]]]]]
         [:BraceClose "}"]]]]]]

    ["{foo{bar}}"
     "{ foo { bar } }"
     " { foo { bar } } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field [:Name "foo"]
           [:SelectionSet
            [:BraceOpen "{"]
            [:Selection [:Field [:Name "bar"]]]
            [:BraceClose "}"]]]]
         [:BraceClose "}"]]]]]]

    ["{foo{bar{foobar}}}"
     "{ foo { bar { foobar } } }"
     " { foo { bar { foobar } } } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field
           [:Name "foo"]
           [:SelectionSet
            [:BraceOpen "{"]
            [:Selection
             [:Field
              [:Name "bar"]
              [:SelectionSet
               [:BraceOpen "{"]
               [:Selection [:Field [:Name "foobar"]]]
               [:BraceClose "}"]]]]
            [:BraceClose "}"]]]]
         [:BraceClose "}"]]]]]]

    ;; TODO: Handle foo,bar in compact case
    ["{foo bar}"
     "{ foo bar }"
     " { foo bar } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "foo"]]]
         [:Selection [:Field [:Name "bar"]]]
         [:BraceClose "}"]]]]]]

    ["{foo{a}bar{a b}foobar{a{b}}}"
     "{ foo { a } bar { a b } foobar { a { b } } }"
     " { foo { a } bar { a b } foobar { a { b } } } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:Field [:Name "foo"]
           [:SelectionSet
            [:BraceOpen "{"]
            [:Selection [:Field [:Name "a"]]]
            [:BraceClose "}"]]]]
         [:Selection
          [:Field
           [:Name "bar"]
           [:SelectionSet
            [:BraceOpen "{"]
            [:Selection [:Field [:Name "a"]]]
            [:Selection [:Field [:Name "b"]]]
            [:BraceClose "}"]]]]
         [:Selection
          [:Field
           [:Name "foobar"]
           [:SelectionSet
            [:BraceOpen "{"]
            [:Selection
             [:Field [:Name "a"]
              [:SelectionSet
               [:BraceOpen "{"]
               [:Selection [:Field [:Name "b"]]]
               [:BraceClose "}"]]]]
            [:BraceClose "}"]]]]
         [:BraceClose "}"]]]]]]

    ["{...frob}"
     "{ ...frob }"
     " { ... frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:FragmentSpread [:Ellipsis "..."] [:FragmentName "frob"]]]
         [:BraceClose "}"]]]]]]

    ["{...frob@foo}"
     "{ ...frob @foo }"
     " { ... frob @ foo } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:FragmentSpread
           [:Ellipsis "..."]
           [:FragmentName "frob"]
           [:Directives [:Directive [:Name "foo"]]]]]
         [:BraceClose "}"]]]]]]

    ["{...foo...bar}"
     "{ ...foo ...bar }"
     " { ... foo ... bar } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:FragmentSpread [:Ellipsis "..."] [:FragmentName "foo"]]]
         [:Selection [:FragmentSpread [:Ellipsis "..."] [:FragmentName "bar"]]]
         [:BraceClose "}"]]]]]]

    ["{...{frob}}"
     "{ ... { frob } }"
     " { ... { frob } } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:InlineFragment
           [:Ellipsis "..."]
           [:SelectionSet
            [:BraceOpen "{"]
            [:Selection [:Field [:Name "frob"]]]
            [:BraceClose "}"]]]]
         [:BraceClose "}"]]]]]]

    ["{...on Foo{...on Bar{foobar}}}"
     "{ ... on Foo { ... on Bar { foobar } } }"
     " { ... on Foo { ... on Bar { foobar } } } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:InlineFragment
           [:Ellipsis "..."]
           [:TypeCondition [:NamedType [:Name "Foo"]]]
           [:SelectionSet
            [:BraceOpen "{"]
            [:Selection
             [:InlineFragment
              [:Ellipsis "..."]
              [:TypeCondition [:NamedType [:Name "Bar"]]]
              [:SelectionSet
               [:BraceOpen "{"]
               [:Selection [:Field [:Name "foobar"]]]
               [:BraceClose "}"]]]]
            [:BraceClose "}"]]]]
         [:BraceClose "}"]]]]]]

    ["{...on Foo@bar{foobar}}"
     "{ ... on Foo @bar { foobar } }"
     " { ... on Foo @ bar { foobar } } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection
          [:InlineFragment
           [:Ellipsis "..."]
           [:TypeCondition [:NamedType [:Name "Foo"]]]
           [:Directives [:Directive [:Name "bar"]]]
           [:SelectionSet
            [:BraceOpen "{"]
            [:Selection [:Field [:Name "foobar"]]]
            [:BraceClose "}"]]]]
         [:BraceClose "}"]]]]]]

    ["query{frob}"
     "query { frob }"
     " query { frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "frob"]]]
         [:BraceClose "}"]]]]]]

    ["mutation{frob}"
     "mutation { frob }"
     " mutation { frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "mutation"]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "frob"]]]
         [:BraceClose "}"]]]]]]

    ["subscription{frob}"
     "subscription { frob }"
     " subscription { frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "subscription"]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "frob"]]]
         [:BraceClose "}"]]]]]]

    ["query frobnicator{frob}"
     "query frobnicator { frob }"
     " query frobnicator { frob } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:Name "frobnicator"]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "frob"]]]
         [:BraceClose "}"]]]]]]

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
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "frob"]]]
         [:BraceClose "}"]]]]]]

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
         [:ParensOpen "("]
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:NamedType [:Name "bar"]]]]
         [:ParensClose ")"]]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]
         [:BraceClose "}"]]]]]]

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
         [:ParensOpen "("]
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:NamedType [:Name "bar"]]]]
         [:VariableDefinition
          [:Variable [:Name "qux"]]
          [:Colon ":"]
          [:Type [:NamedType [:Name "baz"]]]]
         [:ParensClose ")"]]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]
         [:BraceClose "}"]]]]]]

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
         [:ParensOpen "("]
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:ListType
                  [:BracketOpen "["]
                  [:Type [:NamedType [:Name "bar"]]]
                  [:BracketClose "]"]]]]
         [:ParensClose ")"]]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]
         [:BraceClose "}"]]]]]]

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
         [:ParensOpen "("]
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:ListType
                  [:BracketOpen "["]
                  [:Type [:ListType
                          [:BracketOpen "["]
                          [:Type [:NamedType [:Name "bar"]]]
                          [:BracketClose "]"]]]
                  [:BracketClose "]"]]]]
         [:ParensClose ")"]]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]
         [:BraceClose "}"]]]]]]

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
         [:ParensOpen "("]
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:NonNullType [:NamedType [:Name "bar"]] [:ExclamationMark "!"]]]]
         [:ParensClose ")"]]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]
         [:BraceClose "}"]]]]]]

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
         [:ParensOpen "("]
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:NonNullType
                  [:ListType
                   [:BracketOpen "["]
                   [:Type [:NamedType [:Name "bar"]]]
                   [:BracketClose "]"]]
                  [:ExclamationMark "!"]]]]
         [:ParensClose ")"]]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]
         [:BraceClose "}"]]]]]]

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
         [:ParensOpen "("]
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:ListType
                  [:BracketOpen "["]
                  [:Type [:NonNullType
                          [:NamedType [:Name "bar"]]
                          [:ExclamationMark "!"]]]
                  [:BracketClose "]"]]]]
         [:ParensClose ")"]]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]
         [:BraceClose "}"]]]]]]

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
         [:ParensOpen "("]
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
            [:ExclamationMark "!"]]]]
         [:ParensClose ")"]]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]
         [:BraceClose "}"]]]]]]

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
         [:ParensOpen "("]
         [:VariableDefinition
          [:Variable [:Name "foo"]]
          [:Colon ":"]
          [:Type [:NamedType [:Name "bar"]]]
          [:DefaultValue
           [:Equals "="]
           [:Value [:BooleanValue "true"]]]]
         [:ParensClose ")"]]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]
         [:BraceClose "}"]]]]]]

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
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "a"]]]
         [:Selection [:Field [:Name "b"]]]
         [:Selection [:Field [:Name "c"]]]
         [:BraceClose "}"]]]]]]

    ["schema{query:Foo subscription:Bar mutation:Foobar}"
     "schema { query: Foo subscription: Bar mutation: Foobar }"
     " schema { query : Foo subscription : Bar mutation : Foobar } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:SchemaDefinition
        [:BraceOpen "{"]
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
         [:NamedType [:Name "Foobar"]]]
        [:BraceClose "}"]]]]]

    ["schema@foo{query:Foo}"
     "schema @foo { query: Foo }"
     " schema @ foo { query : Foo } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:SchemaDefinition
        [:Directives [:Directive [:Name "foo"]]]
        [:BraceOpen "{"]
        [:RootOperationTypeDefinition
         [:OperationType "query"]
         [:Colon ":"]
         [:NamedType [:Name "Foo"]]]
        [:BraceClose "}"]]]]]

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
           [:StringCharacters
            [:StringCharacter "t"]
            [:StringCharacter "h"]
            [:StringCharacter "e"]
            [:StringCharacter " "]
            [:StringCharacter "s"]
            [:StringCharacter "c"]
            [:StringCharacter "a"]
            [:StringCharacter "l"]
            [:StringCharacter "a"]
            [:StringCharacter "r"]]
           [:Quote]]]
         [:ScalarKeyword "scalar"]
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
           [:StringCharacters
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
            [:StringCharacter "e"]]
           [:Quote]]]
         [:ObjectKeyword "type"]
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
         [:ObjectKeyword "type"]
         [:Name "Foo"]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacters
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
              [:StringCharacter "n"]]
             [:Quote]]]
           [:Name "Bar"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]
           [:Directives [:Directive [:Name "foobar"]]]]
          [:BraceClose "}"]]]]]]]

    ["type Foo{Bar:String}"
     "type Foo { Bar: String }"
     " type Foo { Bar : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:ObjectKeyword "type"]
         [:Name "Foo"]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Name "Bar"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["type Foo{Qux:String Baz:String}"
     "type Foo { Qux: String Baz: String }"
     " type Foo { Qux : String Baz : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:ObjectKeyword "type"]
         [:Name "Foo"]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Name "Qux"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:FieldDefinition
           [:Name "Baz"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["type Foo {Qux: String! Baz(fooArg: String!): String!}"
     "type Foo { Qux : String ! Baz ( fooArg : String ! ) : String ! }"]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:ObjectKeyword "type"]
         [:Name "Foo"]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Name "Qux"]
           [:Colon ":"]
           [:Type
            [:NonNullType [:NamedType [:Name "String"]] [:ExclamationMark "!"]]]]
          [:FieldDefinition
           [:Name "Baz"]
           [:ArgumentsDefinition
            [:ParensOpen "("]
            [:InputValueDefinition
             [:Name "fooArg"]
             [:Colon ":"]
             [:Type
              [:NonNullType [:NamedType [:Name "String"]] [:ExclamationMark "!"]]]]
            [:ParensClose ")"]]
           [:Colon ":"]
           [:Type
            [:NonNullType [:NamedType [:Name "String"]] [:ExclamationMark "!"]]]]
          [:BraceClose "}"]]]]]]]

    ["type Foo implements Bar{qux:String}"
     "type Foo implements Bar { qux: String }"
     " type Foo implements Bar { qux : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:ObjectKeyword "type"]
         [:Name "Foo"]
         [:ImplementsInterfaces
          [:ImplementsKeyword "implements"]
          [:NamedType [:Name "Bar"]]]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Name "qux"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["type Foo implements&Bar{qux:String}"
     "type Foo implements & Bar { qux: String }"
     " type Foo implements & Bar { qux : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:ObjectKeyword "type"]
         [:Name "Foo"]
         [:ImplementsInterfaces
          [:ImplementsKeyword "implements"]
          [:NamedType [:Name "Bar"]]]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Name "qux"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["type Foo implements Bar&Foobar{qux:String}"
     "type Foo implements Bar & Foobar { qux: String }"
     " type Foo implements Bar & Foobar { qux : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:ObjectTypeDefinition
         [:ObjectKeyword "type"]
         [:Name "Foo"]
         [:ImplementsInterfaces
          [:ImplementsInterfaces
           [:ImplementsKeyword "implements"]
           [:NamedType [:Name "Bar"]]]
          [:ImplementsTypeSeparator "&"]
          [:NamedType [:Name "Foobar"]]]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Name "qux"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["interface Foo{qux:String}"
     "interface Foo { qux: String }"
     " interface Foo { qux : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:InterfaceTypeDefinition
         [:InterfaceKeyword "interface"]
         [:Name "Foo"]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Name "qux"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

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
           [:StringCharacters
            [:StringCharacter "t"]
            [:StringCharacter "h"]
            [:StringCharacter "e"]]
           [:Quote]]]
         [:InterfaceKeyword "interface"]
         [:Name "Foo"]
         [:Directives [:Directive [:Name "bar"]]]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacters
              [:StringCharacter "t"]
              [:StringCharacter "h"]
              [:StringCharacter "e"]]
             [:Quote]]]
           [:Name "qux"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:FieldDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacters
              [:StringCharacter "t"]
              [:StringCharacter "h"]
              [:StringCharacter "e"]]
             [:Quote]]]
           [:Name "baz"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["union Foobar"
     " union Foobar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:UnionTypeDefinition
         [:UnionKeyword "union"]
         [:Name "Foobar"]]]]]]

    ["union Foo=Bar"
     " union Foo = Bar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:UnionTypeDefinition
         [:UnionKeyword "union"]
         [:Name "Foo"]
         [:UnionMemberTypes
          [:UnionEqualitySeparator "="]
          [:NamedType [:Name "Bar"]]]]]]]]

    ["union Foobar=Foo|Bar"
     " union Foobar = Foo | Bar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:UnionTypeDefinition
         [:UnionKeyword "union"]
         [:Name "Foobar"]
         [:UnionMemberTypes
          [:UnionMemberTypes
           [:UnionEqualitySeparator "="]
           [:NamedType [:Name "Foo"]]]
          [:UnionTypeSeparator "|"]
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
           [:StringCharacters
            [:StringCharacter "t"]
            [:StringCharacter "h"]
            [:StringCharacter "e"]]
           [:Quote]]]
         [:UnionKeyword "union"]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:UnionMemberTypes
          [:UnionMemberTypes
           [:UnionEqualitySeparator "="]
           [:NamedType [:Name "Foo"]]]
          [:UnionTypeSeparator "|"]
          [:NamedType [:Name "Bar"]]]]]]]]

    ["enum Foobar"
     " enum Foobar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:EnumTypeDefinition
         [:EnumKeyword "enum"]
         [:Name "Foobar"]]]]]]

    ["enum Foobar{FOO}"
     " enum Foobar { FOO } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:EnumTypeDefinition
         [:EnumKeyword "enum"]
         [:Name "Foobar"]
         [:EnumValuesDefinition
          [:BraceOpen "{"]
          [:EnumValueDefinition [:EnumValue "FOO"]]
          [:BraceClose "}"]]]]]]]

    ["enum Foobar{FOO BAR}"
     " enum Foobar { FOO BAR } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:EnumTypeDefinition
         [:EnumKeyword "enum"]
         [:Name "Foobar"]
         [:EnumValuesDefinition
          [:BraceOpen "{"]
          [:EnumValueDefinition [:EnumValue "FOO"]]
          [:EnumValueDefinition [:EnumValue "BAR"]]
          [:BraceClose "}"]]]]]]]

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
           [:StringCharacters
            [:StringCharacter "t"]
            [:StringCharacter "h"]
            [:StringCharacter "e"]]
           [:Quote]]]
         [:EnumKeyword "enum"]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:EnumValuesDefinition
          [:BraceOpen "{"]
          [:EnumValueDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacters
              [:StringCharacter "i"]
              [:StringCharacter "t"]
              [:StringCharacter " "]
              [:StringCharacter "f"]
              [:StringCharacter "o"]
              [:StringCharacter "o"]]
             [:Quote]]]
           [:EnumValue "FOO"]]
          [:EnumValueDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacters
              [:StringCharacter "i"]
              [:StringCharacter "t"]
              [:StringCharacter " "]
              [:StringCharacter "b"]
              [:StringCharacter "a"]
              [:StringCharacter "r"]]
             [:Quote]]]
           [:EnumValue "BAR"]]
          [:BraceClose "}"]]]]]]]

    ["input Foobar"
     " input Foobar "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:InputObjectTypeDefinition
         [:InputKeyword "input"]
         [:Name "Foobar"]]]]]]

    ["input Foobar{foo:String}"
     "input Foobar { foo: String }"
     " input Foobar { foo : String } "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:InputObjectTypeDefinition
         [:InputKeyword "input"]
         [:Name "Foobar"]
         [:InputFieldsDefinition
          [:BraceOpen "{"]
          [:InputValueDefinition
           [:Name "foo"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["input Foobar{foo:String bar:String}"
     "input Foobar { foo: String bar: String}"
     " input Foobar { foo : String bar : String} "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition
        [:InputObjectTypeDefinition
         [:InputKeyword "input"]
         [:Name "Foobar"]
         [:InputFieldsDefinition
          [:BraceOpen "{"]
          [:InputValueDefinition
           [:Name "foo"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:InputValueDefinition
           [:Name "bar"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

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
           [:StringCharacters
            [:StringCharacter "t"]
            [:StringCharacter "h"]
            [:StringCharacter "e"]]
           [:Quote]]]
         [:InputKeyword "input"]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:InputFieldsDefinition
          [:BraceOpen "{"]
          [:InputValueDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacters
              [:StringCharacter "t"]
              [:StringCharacter "h"]
              [:StringCharacter "e"]]
             [:Quote]]]
           [:Name "foo"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]
           [:DefaultValue
            [:Equals "="]
            [:Value
             [:StringValue
              [:Quote]
              [:StringCharacters
               [:StringCharacter "f"]
               [:StringCharacter "o"]
               [:StringCharacter "o"]]
              [:Quote]]]]
           [:Directives [:Directive [:Name "qux"]]]]
          [:InputValueDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacters
              [:StringCharacter "t"]
              [:StringCharacter "h"]
              [:StringCharacter "e"]]
             [:Quote]]]
           [:Name "bar"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]
           [:DefaultValue
            [:Equals "="]
            [:Value
             [:StringValue
              [:Quote]
              [:StringCharacters
               [:StringCharacter "b"]
               [:StringCharacter "a"]
               [:StringCharacter "r"]]
              [:Quote]]]]
           [:Directives [:Directive [:Name "qux"]]]]
          [:BraceClose "}"]]]]]]]

    ["directive@foo on FIELD"
     "directive @foo on FIELD"
     " directive @ foo on FIELD "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:DirectiveDefinition
        [:DirectiveKeyword "directive"]
        [:DirectivePrefix "@"]
        [:Name "foo"]
        [:OnKeyword "on"]
        [:DirectiveLocations
         [:DirectiveLocation [:ExecutableDirectiveLocation "FIELD"]]]]]]]

    ["directive@foo on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive@foo on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive @foo on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT"
     " directive @ foo on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:DirectiveDefinition
        [:DirectiveKeyword "directive"]
        [:DirectivePrefix "@"]
        [:Name "foo"]
        [:OnKeyword "on"]
        [:DirectiveLocations
         [:DirectiveLocations
          [:DirectiveLocations
           [:DirectiveLocation [:ExecutableDirectiveLocation "FIELD"]]]
          [:PipeCharacter "|"]
          [:DirectiveLocation [:ExecutableDirectiveLocation "FRAGMENT_SPREAD"]]]
         [:PipeCharacter "|"]
         [:DirectiveLocation [:ExecutableDirectiveLocation "INLINE_FRAGMENT"]]]]]]]

    ["directive@foo on|FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive @foo on | FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT"
     " directive @ foo on | FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:DirectiveDefinition
        [:DirectiveKeyword "directive"]
        [:DirectivePrefix "@"]
        [:Name "foo"]
        [:OnKeyword "on"]
        [:DirectiveLocations
         [:DirectiveLocations
          [:DirectiveLocations
           [:PipeCharacter "|"]
           [:DirectiveLocation [:ExecutableDirectiveLocation "FIELD"]]]
          [:PipeCharacter "|"]
          [:DirectiveLocation [:ExecutableDirectiveLocation "FRAGMENT_SPREAD"]]]
         [:PipeCharacter "|"]
         [:DirectiveLocation [:ExecutableDirectiveLocation "INLINE_FRAGMENT"]]]]]]]

    ["directive@foo(qux:String baz:String)on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive@foo(qux:String baz:String)on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive @foo(qux: String baz: String) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT"
     " directive @ foo ( qux : String baz : String ) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:DirectiveDefinition
        [:DirectiveKeyword "directive"]
        [:DirectivePrefix "@"]
        [:Name "foo"]
        [:ArgumentsDefinition
         [:ParensOpen "("]
         [:InputValueDefinition
          [:Name "qux"]
          [:Colon ":"]
          [:Type [:NamedType [:Name "String"]]]]
         [:InputValueDefinition
          [:Name "baz"]
          [:Colon ":"]
          [:Type [:NamedType [:Name "String"]]]]
         [:ParensClose ")"]]
        [:OnKeyword "on"]
        [:DirectiveLocations
         [:DirectiveLocations
          [:DirectiveLocations
           [:DirectiveLocation [:ExecutableDirectiveLocation "FIELD"]]]
          [:PipeCharacter "|"]
          [:DirectiveLocation [:ExecutableDirectiveLocation "FRAGMENT_SPREAD"]]]
         [:PipeCharacter "|"]
         [:DirectiveLocation [:ExecutableDirectiveLocation "INLINE_FRAGMENT"]]]]]]]

    ["directive@foo(qux:String baz:String)on|FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive@foo(qux:String baz:String)on |FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT"
     "directive @foo(qux: String baz: String) on | FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT"
     " directive @ foo ( qux : String baz : String ) on | FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT "]
    [:Document
     [:Definition
      [:TypeSystemDefinition
       [:DirectiveDefinition
        [:DirectiveKeyword "directive"]
        [:DirectivePrefix "@"]
        [:Name "foo"]
        [:ArgumentsDefinition
         [:ParensOpen "("]
         [:InputValueDefinition
          [:Name "qux"]
          [:Colon ":"]
          [:Type [:NamedType [:Name "String"]]]]
         [:InputValueDefinition
          [:Name "baz"]
          [:Colon ":"]
          [:Type [:NamedType [:Name "String"]]]]
         [:ParensClose ")"]]
        [:OnKeyword "on"]
        [:DirectiveLocations
         [:DirectiveLocations
          [:DirectiveLocations
           [:PipeCharacter "|"]
           [:DirectiveLocation [:ExecutableDirectiveLocation "FIELD"]]]
          [:PipeCharacter "|"]
          [:DirectiveLocation [:ExecutableDirectiveLocation "FRAGMENT_SPREAD"]]]
         [:PipeCharacter "|"]
         [:DirectiveLocation [:ExecutableDirectiveLocation "INLINE_FRAGMENT"]]]]]]]

    ["extend schema@foo@bar"
     "extend schema @foo @bar"
     " extend schema @ foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:SchemaExtension
        [:ExtendKeyword "extend"]
        [:SchemaKeyword "schema"]
        [:Directives [:Directive [:Name "foo"]] [:Directive [:Name "bar"]]]]]]]

    ["extend schema{query:frobnicate}"
     "extend schema { query: frobnicate }"
     " extend schema { query : frobnicate } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:SchemaExtension
        [:ExtendKeyword "extend"]
        [:SchemaKeyword "schema"]
        [:BraceOpen "{"]
        [:OperationTypeDefinition
         [:OperationType "query"]
         [:Colon ":"]
         [:NamedType [:Name "frobnicate"]]]
        [:BraceClose "}"]]]]]

    ["extend schema@foo{query:frobnicate mutation:frobnitz}"
     "extend schema @foo { query: frobnicate mutation: frobnitz }"
     " extend schema @ foo { query : frobnicate mutation : frobnitz } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:SchemaExtension
        [:ExtendKeyword "extend"]
        [:SchemaKeyword "schema"]
        [:Directives [:Directive [:Name "foo"]]]
        [:BraceOpen "{"]
        [:OperationTypeDefinition
         [:OperationType "query"]
         [:Colon ":"]
         [:NamedType [:Name "frobnicate"]]]
        [:OperationTypeDefinition
         [:OperationType "mutation"]
         [:Colon ":"]
         [:NamedType [:Name "frobnitz"]]]
        [:BraceClose "}"]]]]]

    ["extend scalar Foo@bar"
     "extend scalar Foo @bar"
     " extend scalar Foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ScalarTypeExtension
         [:ExtendKeyword "extend"]
         [:ScalarKeyword "scalar"]
         [:Name "Foo"]
         [:Directives [:Directive [:Name "bar"]]]]]]]]

    "extend type Foo implements Qux"
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ObjectTypeExtension
         [:ExtendKeyword "extend"]
         [:TypeKeyword "type"]
         [:Name "Foo"]
         [:ImplementsInterfaces
          [:ImplementsKeyword "implements"]
          [:NamedType [:Name "Qux"]]]]]]]]

    ["extend type Foo implements Qux&Baz"
     " extend type Foo implements Qux & Baz "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ObjectTypeExtension
         [:ExtendKeyword "extend"]
         [:TypeKeyword "type"]
         [:Name "Foo"]
         [:ImplementsInterfaces
          [:ImplementsInterfaces
           [:ImplementsKeyword "implements"]
           [:NamedType [:Name "Qux"]]]
          [:ImplementsTypeSeparator "&"]
          [:NamedType [:Name "Baz"]]]]]]]]

    ["extend type Foo@bar"
     "extend type Foo @bar"
     " extend type Foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ObjectTypeExtension
         [:ExtendKeyword "extend"]
         [:TypeKeyword "type"]
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
         [:ExtendKeyword "extend"]
         [:TypeKeyword "type"]
         [:Name "Foo"]
         [:ImplementsInterfaces
          [:ImplementsInterfaces
           [:ImplementsKeyword "implements"]
           [:NamedType [:Name "Qux"]]]
          [:ImplementsTypeSeparator "&"]
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
         [:ExtendKeyword "extend"]
         [:TypeKeyword "type"]
         [:Name "Foo"]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Name "qux"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["extend type Foo implements Bar@foobar{\"the\"qux:String@baz}"
     "extend type Foo implements Bar @foobar { \"the\" qux: String @baz }"
     " extend type Foo implements Bar @ foobar { \"the\" qux : String @ baz } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:ObjectTypeExtension
         [:ExtendKeyword "extend"]
         [:TypeKeyword "type"]
         [:Name "Foo"]
         [:ImplementsInterfaces
          [:ImplementsKeyword "implements"]
          [:NamedType [:Name "Bar"]]]
         [:Directives [:Directive [:Name "foobar"]]]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Description
            [:StringValue
             [:Quote]
             [:StringCharacters
              [:StringCharacter "t"]
              [:StringCharacter "h"]
              [:StringCharacter "e"]]
             [:Quote]]]
           [:Name "qux"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]
           [:Directives [:Directive [:Name "baz"]]]]
          [:BraceClose "}"]]]]]]]

    ["extend interface Foo@bar"
     "extend interface Foo @bar"
     " extend interface Foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:InterfaceTypeExtension
         [:ExtendKeyword "extend"]
         [:InterfaceKeyword "interface"]
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
         [:ExtendKeyword "extend"]
         [:InterfaceKeyword "interface"]
         [:Name "Foobar"]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Name "foo"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:FieldDefinition
           [:Name "bar"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["extend interface Foobar@foo{bar:String}"
     "extend interface Foobar @foo { bar: String }"
     " extend interface Foobar @ foo { bar : String } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:InterfaceTypeExtension
         [:ExtendKeyword "extend"]
         [:InterfaceKeyword "interface"]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "foo"]]]
         [:FieldsDefinition
          [:BraceOpen "{"]
          [:FieldDefinition
           [:Name "bar"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["extend union Foobar@foo@bar"
     "extend union Foobar @foo @bar"
     " extend union Foobar @ foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:UnionTypeExtension
         [:ExtendKeyword "extend"]
         [:UnionKeyword "union"]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "foo"]] [:Directive [:Name "bar"]]]]]]]]

    ["extend union Foobar=Qux|Baz"
     " extend union Foobar = Qux | Baz "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:UnionTypeExtension
         [:ExtendKeyword "extend"]
         [:UnionKeyword "union"]
         [:Name "Foobar"]
         [:UnionMemberTypes
          [:UnionMemberTypes
           [:UnionEqualitySeparator "="]
           [:NamedType [:Name "Qux"]]]
          [:UnionTypeSeparator "|"]
          [:NamedType [:Name "Baz"]]]]]]]]

    ["extend union Foobar@qux=Baz"
     "extend union Foobar @qux = Baz"
     " extend union Foobar @ qux = Baz "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:UnionTypeExtension
         [:ExtendKeyword "extend"]
         [:UnionKeyword "union"]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:UnionMemberTypes
          [:UnionEqualitySeparator "="]
          [:NamedType [:Name "Baz"]]]]]]]]

    ["extend enum Foobar@foo@bar"
     "extend enum Foobar @foo @bar"
     " extend enum Foobar @ foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:EnumTypeExtension
         [:ExtendKeyword "extend"]
         [:EnumKeyword "enum"]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "foo"]] [:Directive [:Name "bar"]]]]]]]]

    ["extend enum Foobar{QUX BAZ}"
     " extend enum Foobar { QUX BAZ } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:EnumTypeExtension
         [:ExtendKeyword "extend"]
         [:EnumKeyword "enum"]
         [:Name "Foobar"]
         [:EnumValuesDefinition
          [:BraceOpen "{"]
          [:EnumValueDefinition [:EnumValue "QUX"]]
          [:EnumValueDefinition [:EnumValue "BAZ"]]
          [:BraceClose "}"]]]]]]]

    ["extend enum Foobar@qux{BAZ}"
     "extend enum Foobar @qux { BAZ }"
     " extend enum Foobar @ qux { BAZ } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:EnumTypeExtension
         [:ExtendKeyword "extend"]
         [:EnumKeyword "enum"]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:EnumValuesDefinition
          [:BraceOpen "{"]
          [:EnumValueDefinition [:EnumValue "BAZ"]]
          [:BraceClose "}"]]]]]]]

    ["extend input Foobar@foo@bar"
     "extend input Foobar @foo @bar"
     " extend input Foobar @ foo @ bar "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:InputObjectTypeExtension
         [:ExtendKeyword "extend"]
         [:InputKeyword "input"]
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
         [:ExtendKeyword "extend"]
         [:InputKeyword "input"]
         [:Name "Foobar"]
         [:InputFieldsDefinition
          [:BraceOpen "{"]
          [:InputValueDefinition
           [:Name "qux"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:InputValueDefinition
           [:Name "baz"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["extend input Foobar@qux{baz:String}"
     "extend input Foobar @qux { baz: String }"
     " extend input Foobar @ qux { baz : String } "]
    [:Document
     [:Definition
      [:TypeSystemExtension
       [:TypeExtension
        [:InputObjectTypeExtension
         [:ExtendKeyword "extend"]
         [:InputKeyword "input"]
         [:Name "Foobar"]
         [:Directives [:Directive [:Name "qux"]]]
         [:InputFieldsDefinition
          [:BraceOpen "{"]
          [:InputValueDefinition
           [:Name "baz"]
           [:Colon ":"]
           [:Type [:NamedType [:Name "String"]]]]
          [:BraceClose "}"]]]]]]]

    ["query{foo}{foo:String}fragment foo on Bar{foo}type Foo schema{query:Foo}"
     "query {foo} { foo : String } fragment foo on Bar {foo} type Foo schema {query:Foo}"
     " query { foo } { foo : String } fragment foo on Bar { foo } type Foo schema { query : Foo } "]
    [:Document
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:OperationType "query"]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "foo"]]]
         [:BraceClose "}"]]]]]
     [:Definition
      [:ExecutableDefinition
       [:OperationDefinition
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field
                      [:Alias [:Name "foo"] [:Colon ":"]]
                      [:Name "String"]]]
         [:BraceClose "}"]]]]]
     [:Definition
      [:ExecutableDefinition
       [:FragmentDefinition
        [:FragmentName "foo"]
        [:TypeCondition [:NamedType [:Name "Bar"]]]
        [:SelectionSet
         [:BraceOpen "{"]
         [:Selection [:Field [:Name "foo"]]]
         [:BraceClose "}"]]]]]
     [:Definition
      [:TypeSystemDefinition
       [:TypeDefinition [:ObjectTypeDefinition [:ObjectKeyword "type"] [:Name "Foo"]]]]]
     [:Definition
      [:TypeSystemDefinition
       [:SchemaDefinition
        [:BraceOpen "{"]
        [:RootOperationTypeDefinition
         [:OperationType "query"]
         [:Colon ":"]
         [:NamedType [:Name "Foo"]]]
        [:BraceClose "}"]]]]]))

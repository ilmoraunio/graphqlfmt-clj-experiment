(ns graphql-fmt.print-test
  (:refer-clojure :exclude [name comment])
  (:require [clojure.test :refer [are deftest is testing]]
            [graphql-fmt.core :refer :all :as graphqlfmt]
            [instaparse.core :as insta]))

#_(deftest test-unformatted-output
  (are [graphql]
       (let [is-a-vec? (vector? graphql)
             expected (if is-a-vec? (second graphql) graphql)
             input (if is-a-vec? (first graphql) graphql)]
         (amend-indentation-level-opts
           0
           (amend-newline-spacing
             (amend-newline-opts
               (insta/transform
                 transform-map
                 (document-parser input)))))
         (= expected
            (pr-str-ast
              ""
              (insta/transform
                transform-map
                (document-parser input)))))
    "{frob@foo}"
    "{frob@foo @bar}"
    "{frob@foo(bar:true)}"
    "{frob@foo(a:1,b:2)}"
    "{frob@foo(a:1) @bar(a:1)}"
    "{foo{bar}}"
    "{foo{bar{foobar}}}"
    "{foo,bar}"
    "{foo{a},bar{a,b},foobar{a{b}}}"
    "{...frob}"
    "{...frob@foo}"
    "{...foo,...bar}"
    "{...{frob}}"
    "{...on Foo{...on Bar{foobar}}}"
    "{...on Foo@bar{foobar}}"
    "query {frob}"
    "mutation {frob}"
    "subscription {frob}"
    "query frobnicator{frob}"
    "query frobnicator@foo{frob}"
    "query frob($foo:bar) {a,b}"
    "query frob($foo:bar$qux:baz) {a,b}"
    "query frob($foo:[bar]) {a,b}"
    "query frob($foo:[[bar]]) {a,b}"
    "query frob($foo:bar!) {a,b}"
    "query frob($foo:[bar]!) {a,b}"
    "query frob($foo:[bar!]) {a,b}"
    "query frob($foo:[bar!]!) {a,b}"
    "query frob($foo:bar=true) {a,b}"
    "fragment foo on Bar @foobar {a,b,c}"
    "schema {query:Foo,subscription:Bar,mutation:Foobar}"
    "schema @foo{query:Foo}"
    "\"the scalar\" scalar Foo @bar"
    "\"documents the\" type Foo @bar"
    "type Foo { \"the field definition\" Bar : String @foobar }"
    "type Foo { Bar : String }"
    "type Foo { Qux : String Baz : String }"
    "type Foo {Qux: String! Baz(fooArg: String!): String!}"
    "type Foo implements Bar { qux : String }"
    "type Foo implements & Bar { qux : String }"
    "type Foo implements Bar & Foobar { qux : String }"
    "interface Foo { qux : String }"
    "\"the\" interface Foo @bar { \"the\" qux : String \"the\" baz : String }"
    ["\"the\"interface Foo@bar{\"the\"qux:String\"the\"baz:String}"
     "\"the\" interface Foo @bar { \"the\" qux : String \"the\" baz : String }"]
    "union Foobar"
    "union Foo = Bar"
    "union Foobar = Foo | Bar"
    "\"the\" union Foobar @qux = Foo | Bar"
    "enum Foobar"
    "enum Foobar { FOO }"
    "enum Foobar { FOO BAR }"
    "\"the\" enum Foobar @qux { \"it foo\" FOO \"it bar\" BAR }"
    "input Foobar"
    "input Foobar { foo : String }"
    "input Foobar { foo : String bar : String }"
    "\"the\" input Foobar @qux { \"the\" foo : String =\"foo\" @qux \"the\" bar : String =\"bar\" @qux }"
    "directive @ foo on FIELD"
    "directive @ foo on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT"
    "directive @ foo ( qux : String baz : String ) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT"
    "extend schema @foo @bar"
    "extend schema { query : frobnicate }"
    "extend schema @foo { query : frobnicate mutation : frobnitz }"
    "extend scalar Foo @bar"
    "extend type Foo implements Qux & Baz"
    "extend type Foo @bar"
    "extend type Foo implements Qux & Baz @bar"
    "extend type Foo { qux : String }"
    "extend type Foo implements Bar @foobar { \"the\" qux : String @baz }"
    "extend interface Foo @bar"
    "extend interface Foobar { foo : String bar : String }"
    "extend interface Foobar @foo { bar : String }"
    "extend union Foobar @foo @bar"
    "extend union Foobar = Qux | Baz"
    "extend union Foobar @qux = Baz"
    "extend enum Foobar @foo @bar"
    "extend enum Foobar { QUX BAZ }"
    "extend enum Foobar @qux { BAZ }"
    "extend input Foobar @foo @bar"
    "extend input Foobar { qux : String baz : String }"
    "extend input Foobar @qux { baz : String }"
    "query {foo} {foo:String} fragment foo on Bar {foo} type Foo schema {query:Foo}"))

(def graphql-statements
  (->> (clojure.java.io/file "test-resources/graphql")
    (file-seq)
    (filter #(.isFile %))
    (map (juxt #(.getName %) slurp))
    (mapv (fn [[filename graphql]] {:filename filename
                                    :graphql graphql}))))

(defmacro run-tests
  []
  `(do (clojure.template/do-template
         [graphql-statement]
         (let [graphql# (:graphql graphql-statement)]
           (prn (format "testing: %s" (:filename graphql-statement)))
           (is (= graphql# (graphqlfmt/fmt graphql#))
               (:filename graphql-statement)))
         ~@graphql-statements)))

(deftest test-formatted-output (run-tests))
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
    "query {foo} {foo:String} fragment foo on Bar {foo} type Foo schema {query:Foo}"
    ;;
    ;; we need to have a test that tests for exact input and output
    "type Foo implements & Bar { qux : String }"
    ))

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
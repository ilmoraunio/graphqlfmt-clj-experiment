(ns evolution-of-the-ast
  (:require [graphqlfmt.ast :as ast]
            [graphqlfmt.core :as core]
            [graphqlfmt.options :as options]
            [graphqlfmt.print :as print]
            [graphqlfmt.transform :as transform]))

;; 1. Instaparse output

(->> "{foo(bar: 1)}" ast/document-parser)

;; 2. Vectors with options

(->> "{foo(bar: 1)}" ast/parse)

;; 3. Emit options & apply re-transformation
;;    eg. block string characters, indentation,
;;        horizontal spacing

(->> "{foo(bar: 1)}"
     ast/parse
     transform/transform
     options/amend-options
     transform/re-transform)

;; 4. Change to row-based AST & recalculate some opts

(->> "{foo(bar: 1)}"
     ast/parse
     transform/transform
     options/amend-options
     transform/re-transform
     transform/row-ast
     options/amend-characters-opts)

;; 5. Formatted output

(print (->> "{foo(bar: 1)}"
            ast/parse
            transform/transform
            options/amend-options
            transform/re-transform
            transform/row-ast
            options/amend-characters-opts
            print/pr-s))

;; non-trivial wrapping example

(print (core/fmt "
query Foo($a: String = \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\") {
  a(a: \"a\")
}
"))

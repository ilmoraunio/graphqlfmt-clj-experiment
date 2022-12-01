(ns evolution-of-the-ast
  (:require [graphqlfmt.ast :as ast]
            [graphqlfmt.core :as core]
            [graphqlfmt.options :as options]
            [graphqlfmt.print :as print]
            [graphqlfmt.transform :as transform]
            [instaparse.core :as insta]))

;; 1. Instaparse output

(->> "{foo(bar: 1)}" ast/document-parser)

;; 2. Transform into "enhanced" hiccup style

(->> "{foo(bar: 1)}"
     ast/document-parser
     (insta/transform ast/transform-map))

;; 3. Emit options

(->> "{foo(bar: 1)}"
     ast/parse
     ;
     options/amend-newline-opts
     options/amend-indentation-level-opts
     options/amend-horizontal-spacing-opts
     #_…)

;; 4. Re-transform based on options

(->> "{foo(bar: 1)}"
     ast/parse
     options/amend-options
     ;
     transform/format-block-string-values
     transform/amend-newline-spacing
     transform/amend-horizontal-spacing
     transform/amend-softline)

;; 5. Change to row-based AST

(->> "{foo(bar: 1)}"
     ast/parse
     options/amend-options
     transform/re-transform
     ;
     transform/row-ast
     options/amend-characters-opts)

;; 6. Formatted output

(->> "{foo(bar: 1)}"
     ast/parse
     options/amend-options
     transform/re-transform
     transform/row-ast
     options/amend-characters-opts
     ;
     print/pr-s
     print)

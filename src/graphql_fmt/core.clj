(ns graphql-fmt.core
  (:refer-clojure :exclude [name comment format])
  (:require [clojure.java.io :as io]
            [clojure.pprint]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [instaparse.transform :as insta-transform])
  (:gen-class))

(defn ebnf [& names]
  (apply str (map (fn [name]
                    (-> (str name ".ebnf")
                      io/resource
                      io/reader
                      slurp))
                  names)))

(def ignored-parser
  (insta/parser (ebnf "ignored")))

(def token-parser
  (insta/parser (ebnf "token")))

(def document-parser
  (insta/parser (ebnf "document" "token" "ignored")))

(defn comment
  ([] [:Comment {} "#"])
  ([x] [:Comment {} (str "# " x)])
  ([x & ys] [:Comment {} (str "# " x (apply str ys))]))

(defn document [x]
  [:Document {} x])

(defn boolean-value [x]
  [:BooleanValue {} x])

(defn selection
  ([& xs] (reduce (fn [coll x] (conj coll x))
                  [:Selection {}]
                  xs)))

(defn float-value [x & ys]
  [:FloatValue {} (str x (apply str ys))])

(defn int-value [x]
  [:IntValue {} x])

(defn null-value []
  [:NullValue {} "null"])

(defn string-value [& xs]
  [:StringValue {} (apply str xs)])

(def transform-map
  {:Alias (fn [& xs]
            (reduce (fn [coll x] (conj coll x))
                    [:Alias {}]
                    xs))
   :Argument (fn [& xs] (reduce (fn [coll x]
                                  (cond-> coll
                                    true (conj x)
                                    (= :Colon (first x)) (conj [:Printable {} " "])))
                                [:Argument {}]
                                xs))
   :Arguments (fn [& xs]
                (reduce
                  (fn [coll x] (conj coll x))
                  [:Arguments {}]
                  (conj (into [[:Printable {} "("]]
                              (interpose [:Printable {} ","] xs))
                        [:Printable {} ")"])))
   :ArgumentsDefinition (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:ArgumentsDefinition {}]
                                  (interpose [:Printable {} " "] xs)))
   :BlockQuote (fn [] "\"\"\"")
   :BlockStringCharacter str
   :BooleanValue boolean-value
   :BraceClose (fn [x] [:BraceClose {} x])
   :BraceOpen (fn [x] [:BraceOpen {} x])
   :BracketClose (fn [x] [:Printable {} x])
   :BracketOpen (fn [x] [:Printable {} x])
   :Colon (fn [x] [:Colon {} x])
   :Comment comment
   :CommentChar str
   :DefaultValue (fn [& xs]
                   (reduce (fn [coll x] (conj coll x))
                           [:DefaultValue {}]
                           xs))
   :Definition (fn [x] [:Definition {} x])
   :Description (fn [x] [:Description {} x])
   :Digit str
   :Directive (fn [& xs]
                (reduce (fn [coll x] (conj coll x))
                        [:Directive {}]
                        (into [[:Printable {} "@"]] xs)))
   :DirectiveDefinition (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:DirectiveDefinition {}]
                                  (conj (interpose [:Printable {} " "] xs))))
   :DirectiveKeyword (fn [x] [:Printable {} x])
   :DirectiveLocation (fn [& xs]
                        (reduce (fn [coll x] (conj coll x))
                                [:DirectiveLocation {}]
                                (conj (interpose [:Printable {} " "] xs))))
   :DirectiveLocations (fn [& xs]
                         (reduce (fn [coll x] (conj coll x))
                                 [:DirectiveLocations {}]
                                 (conj (interpose [:Printable {} " "] xs))))
   :DirectivePrefix (fn [x] [:Printable {} x])
   :Directives (fn [& xs]
                 (reduce (fn [coll x] (conj coll x))
                         [:Directives {}]
                         (conj (interpose [:Printable {} " "] xs))))
   :Document (fn [& xs]
               (reduce (fn [coll x] (conj coll x))
                       [:Document {}]
                       (conj (interpose [:Printable {} " "] xs))))
   :ExecutableDirectiveLocation (fn [x] [:ExecutableDirectiveLocation {} x])
   :EnumKeyword (fn [x] [:Printable {} x])
   :EnumTypeDefinition (fn [& xs]
                         (reduce (fn [coll x] (conj coll x))
                                 [:EnumTypeDefinition {}]
                                 (conj (interpose [:Printable {} " "] xs))))
   :EnumTypeExtension (fn [& xs]
                        (reduce (fn [coll x] (conj coll x))
                                [:EnumTypeExtension {}]
                                (conj (interpose [:Printable {} " "] xs))))
   :EnumValue (fn [x] [:EnumValue {} x])
   :EnumValueDefinition (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:EnumValueDefinition {}]
                                  (conj (interpose [:Printable {} " "] xs))))
   :EnumValuesDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:EnumValuesDefinition {}]
                                   (conj (interpose [:Printable {} " "] xs))))
   :Equals (fn [x] [:Printable {} x])
   :EscapedCharacter str
   :EscapedUnicode str
   :ExclamationMark (fn [x] [:Printable {} x])
   :ExecutableDefinition (fn [x] [:ExecutableDefinition {} x])
   :ExponentIndicator str
   :ExponentPart str
   :ExtendKeyword (fn [x] [:Printable {} x])
   :Field (fn [& xs]
            (reduce (fn [coll x]
                      (conj coll (case (first x)
                                   :Alias (conj x [:Printable {} " "])
                                   x)))
                    [:Field {}]
                    xs))
   :FieldDefinition (fn [& xs]
                      (reduce (fn [coll x] (conj coll x))
                              [:FieldDefinition {}]
                              (interpose [:Printable {} " "] xs)))
   :FieldNameSeparator (fn [] [:Printable {} ":"])
   :FieldsDefinition (fn [& xs]
                       (conj (reduce (fn [coll x] (conj coll x))
                                     [:FieldsDefinition {}]
                                     (interpose [:Printable {} " "] xs))))
   :FloatValue float-value
   :FragmentDefinition (fn [& xs]
                         (reduce (fn [coll x] (conj coll x))
                                 [:FragmentDefinition {}
                                  [:Printable {} "fragment"]
                                  [:Printable {} " "]]
                                 (conj (interpose [:Printable {} " "] xs))))
   :FractionalPart (partial str ".")
   :FragmentName (fn [s] [:Printable {} s])
   :FragmentSpread (fn [& xs]
                     (reduce (fn [coll x] (conj coll x))
                             [:FragmentSpread {} [:Printable {} "..."]]
                             xs))
   :ImplementsInterfaces (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:ImplementsInterfaces {}]
                                   (conj (interpose [:Printable {} " "] xs))))
   :ImplementsKeyword (fn [x] [:Printable {} x])
   :ImplementsTypeSeparator (fn [x] [:Printable {} x])
   :InlineFragment (fn [& xs]
                     (reduce (fn [coll x] (conj coll x))
                             [:InlineFragment {} [:Printable {} "..."]]
                             xs))
   :InputValueDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:InputValueDefinition {}]
                                   (conj (interpose [:Printable {} " "] xs))))
   :InputFieldsDefinition (fn [& xs]
                            (reduce (fn [coll x] (conj coll x))
                                    [:InputFieldsDefinition {}]
                                    (conj (interpose [:Printable {} " "] xs))))
   :InputKeyword (fn [x] [:Printable {} x])
   :InputObjectTypeDefinition (fn [& xs]
                                (reduce (fn [coll x] (conj coll x))
                                        [:InputObjectTypeDefinition {}]
                                        (conj (interpose [:Printable {} " "] xs))))
   :InputObjectTypeExtension (fn [& xs]
                               (reduce (fn [coll x] (conj coll x))
                                       [:InputObjectTypeExtension {}]
                                       (conj (interpose [:Printable {} " "] xs))))
   :IntValue int-value
   :IntegerPart str
   :InterfaceKeyword (fn [x] [:Printable {} x])
   :InterfaceTypeDefinition (fn [& xs]
                              (reduce (fn [coll x] (conj coll x))
                                      [:InterfaceTypeDefinition {}]
                                      (conj (interpose [:Printable {} " "] xs))))
   :InterfaceTypeExtension (fn [& xs]
                             (reduce (fn [coll x] (conj coll x))
                                     [:InterfaceTypeExtension {}]
                                     (conj (interpose [:Printable {} " "] xs))))
   :ListType (fn [& xs]
               (reduce (fn [coll x] (conj coll x))
                       [:ListType {}]
                       xs))
   :ListValue (fn [& xs]
                (conj (reduce
                        (fn [coll x] (conj coll x))
                        [:ListValue {} [:Printable {} "["]]
                        (conj (interpose [:Printable {} ","] xs)))
                      [:Printable {} "]"]))
   :Name (fn [x] [:Name {} x])
   :NamedType (fn [x] [:NamedType {} x])
   :NegativeSign str
   :NonNullType (fn [& xs]
                  (reduce (fn [coll x] (conj coll x))
                          [:NonNullType {}]
                          xs))
   :NonZeroDigit str
   :NullValue null-value
   :ObjectField (fn [x & xs]
                  (reduce (fn [coll x] (conj coll x))
                          [:ObjectField {}
                           x
                           [:Printable {} ":"]]
                          xs))
   :ObjectKeyword (fn [x] [:Printable {} x])
   :ObjectTypeDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:ObjectTypeDefinition {}]
                                   (interpose [:Printable {} " "] xs)))
   :ObjectTypeExtension (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:ObjectTypeExtension {}]
                                  (interpose [:Printable {} " "] xs)))
   :ObjectValue (fn [& xs]
                  (conj (reduce
                          (fn [coll x] (conj coll x))
                          [:ListValue {} [:Printable {} "{"]]
                          (conj (interpose [:Printable {} ","] xs)))
                        [:Printable {} "}"]))
   :OnKeyword (fn [x] [:Printable {} x])
   :OperationDefinition (fn [& xs]
                          (reduce (fn [coll x]
                                    (if (= (first x) :OperationType)
                                      (conj coll x [:Printable {} " "])
                                      (conj coll x)))
                                  [:OperationDefinition {}]
                                  xs))
   :OperationType (fn [x] [:OperationType {} [:Printable {} x]])
   :OperationTypeDefinition (fn [& xs]
                              (reduce (fn [coll x] (conj coll x))
                                      [:OperationTypeDefinition {}]
                                      (interpose [:Printable {} " "] xs)))
   :ParensOpen (fn [x] [:Printable {} x])
   :ParensClose (fn [x] [:Printable {} x])
   :PipeCharacter (fn [x] [:Printable {} x])
   :Quote (fn [] "\"")
   :RootOperationTypeDefinition (fn [& xs]
                                  (reduce (fn [coll x] (conj coll x))
                                          [:RootOperationTypeDefinition {}]
                                          xs))
   :SchemaExtension (fn [& xs]
                      (reduce (fn [coll x] (conj coll x))
                              [:SchemaExtension {}]
                              (conj (interpose [:Printable {} " "] xs))))
   :SchemaKeyword (fn [x] [:Printable {} x])
   :ScalarKeyword (fn [x] [:Printable {} x])
   :ScalarTypeDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:ScalarTypeDefinition {}]
                                   (interpose [:Printable {} " "] xs)))
   :ScalarTypeExtension (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:ScalarTypeExtension {}]
                                  (interpose [:Printable {} " "] xs)))
   :SchemaDefinition (fn [& xs]
                       (reduce (fn [coll x]
                                 (if (and (= (first (last coll))
                                             :RootOperationTypeDefinition)
                                          (= (first x)
                                             :RootOperationTypeDefinition))
                                   (conj coll [:Printable {} ","] x)
                                   (conj coll x)))
                               [:SchemaDefinition {}
                                [:Printable {} "schema"]
                                [:Printable {} " "]]
                               xs))
   :Selection selection
   :SelectionSet (fn [& xs]
                   (reduce
                     (fn [coll x] (conj coll x))
                     [:SelectionSet {}]
                     xs))
   :Sign str
   :StringCharacter str
   :StringValue string-value
   :Type (fn [& xs]
           (reduce (fn [coll x] (conj coll x))
                   [:Type {}]
                   xs))
   :TypeCondition (fn [& xs]
                    (reduce (fn [coll x] (conj coll x))
                            [:TypeCondition {}
                             [:Printable {} "on"]
                             [:Printable {} " "]]
                            xs))
   :TypeDefinition (fn [x] [:TypeDefinition {} x])
   :TypeExtension (fn [& xs]
                    (reduce (fn [coll x] (conj coll x))
                            [:TypeExtension {}]
                            xs))
   :TypeKeyword (fn [x] [:Printable {} x])
   :TypeSystemDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:TypeSystemDefinition {}]
                                   xs))
   :TypeSystemExtension (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:TypeSystemExtension {}]
                                  xs))
   :UnionEqualitySeparator (fn [x] [:Printable {} x])
   :UnionKeyword (fn [x] [:Printable {} x])
   :UnionMemberTypes (fn [& xs]
                       (reduce (fn [coll x] (conj coll x))
                               [:UnionMemberTypes {}]
                               (interpose [:Printable {} " "] xs)))
   :UnionTypeDefinition (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:UnionTypeDefinition {}]
                                  (interpose [:Printable {} " "] xs)))
   :UnionTypeExtension (fn [& xs]
                         (reduce (fn [coll x] (conj coll x))
                                 [:UnionTypeExtension {}]
                                 (interpose [:Printable {} " "] xs)))
   :UnionTypeSeparator (fn [x] [:Printable {} x])
   :Value (fn [& xs]
            (reduce (fn [coll x] (conj coll x))
                    [:Value {}]
                    xs))
   :Variable (fn [x] [:Variable {} [:Printable {} "$"] x])
   :VariableDefinition (fn [& xs]
                         (reduce (fn [coll x] (conj coll x))
                                 [:VariableDefinition {}]
                                 xs))
   :VariableDefinitions (fn [& xs]
                          (conj (reduce (fn [coll x] (conj coll x))
                                        [:VariableDefinitions {}
                                         [:Printable {} "("]]
                                        xs)
                                [:Printable {} ")"]
                                [:Printable {} " "]))})

(def template
  [:Document {}
   [:Definition {}
    [:ExecutableDefinition {}
     [:OperationDefinition {}
      [:SelectionSet {}
       [:Printable {} "{"]
       [:Selection {}
        [:Field {}
         [:Name {
                 :newline? true} "foo"]]]
       [:Printable {} "}"]]]]]
   [:Definition {}
    [:Printable {} "{"]
    [:Printable {} "bar"]
    [:Printable {} "}"]]])

(def desired-format
  [:Document {:indentation-level 0}
   [:Definition {:indentation-level 0}
    [:ExecutableDefinition {:indentation-level 0}
     [:OperationDefinition {:indentation-level 0}
      [:SelectionSet {:indentation-level 0}
       [:Printable {:indentation-level 0} "{"]
       [:Selection {:indentation-level 1}
        [:Field {:indentation-level 1}
         [:Name {:indentation-level 1,
                 :newline? true} "foo"]]]
       [:Printable {} "}"]]]]]
   [:Definition {:indentation-level 0}
    [:Printable {} "{"]
    [:Printable {} "bar"]
    [:Printable {} "}"]]])

(defn pr-str-ast
  [s ast]
  (let [[_node _opts & rst] ast]
    (apply str (cond
                 (vector? (first rst)) (map (partial pr-str-ast s) rst)
                 (string? (first rst)) (str s (first rst))))))

;; validate ast fns

(defn validate-ast-form
  [ast]
  (let [[node opts & rst] ast]
    (when (and (not (vector? (first rst)))
               (not (string? (first rst))))
      (throw (ex-info {} (str "Unrecognized type: " (type (first rst))))))
    (into [node opts]
          (cond
            (vector? (first rst)) (map validate-ast-form rst)
            (string? (first rst)) rst))))

;; enrich ast opts fns

(defn amend-indentation-level
  [indent-level ast]
  (let [[node opts & rst] ast]
    (let [indent-level (case node
                         (:Selection :Arguments) (inc indent-level)
                         indent-level)]
      (into [node (into opts {:indentation-level indent-level})]
            (cond
              (vector? (first rst)) (map (partial amend-indentation-level indent-level) rst)
              (string? (first rst)) rst)))))

(defn amend-newline-opts
  [ast]
  (let [[node opts & rst] ast]
    (into [node (if (condp = node
                      :Selection true
                      :BraceClose true
                      :Argument true
                      false)
                  (into opts {:newline? true})
                  opts)]
          (cond
            (vector? (first rst)) (map amend-newline-opts rst)
            (string? (first rst)) rst))))

(defn amend-horizontal-spacing-opts
  [ast]
  (let [[node opts & rst] ast]
    (into [node (if (condp = node
                      :Alias true
                      false)
                  (into opts {:append-whitespace? true})
                  opts)]
          (cond
            (vector? (first rst)) (map amend-horizontal-spacing-opts rst)
            (string? (first rst)) rst))))

;; enrich-ast fns

(defn amend-newline-spacing
  [ast]
  (let [[node opts & rst] ast]
    (into (if (:newline? opts)
            (into [node opts [:Printable {} "\n"]]
                  [[:Printable {}
                    (->> " "
                      (repeat
                        (* 2 (:indentation-level opts)))
                      (apply str))]])
            [node opts])
          (cond
            (vector? (first rst)) (map amend-newline-spacing rst)
            (string? (first rst)) (if (:newline? opts)
                                    [[:Printable {} (first rst)]]
                                    rst)))))

(defn amend-horizontal-spacing
  [ast]
  (let [[node opts & rst] ast]
    (into (if (:append-whitespace? opts)
            (cond-> [node opts]
              (= :Alias node) (into [[:Printable {} " "]]))
            [node opts])
          (cond
            (vector? (first rst)) (map amend-horizontal-spacing rst)
            (string? (first rst)) rst))))

(defn ast
  [s]
  (->> s
    document-parser
    (insta/transform transform-map)))

(defn enrich-ast-opts
  [ast]
  (->> ast
    amend-newline-opts
    (amend-indentation-level 0)
    (amend-horizontal-spacing-opts)))

(defn enrich-ast
  [ast]
  (->> ast
    (amend-newline-spacing)))

(defn format
  [s]
  (->> s
    ast
    validate-ast-form
    enrich-ast-opts
    enrich-ast
    (pr-str-ast "")
    (clojure.core/format "%s\n")))

(defn -main [& args]
  (let [graphql (or (first args) (apply str (line-seq (java.io.BufferedReader. *in*))))
        output (format graphql)]
    (print output)
    (flush)))

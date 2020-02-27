(ns graphql-fmt.core
  (:refer-clojure :exclude [name comment format])
  (:require [clojure.java.io :as io]
            [clojure.pprint]
            [clojure.string :as str]
            [instaparse.core :as insta])
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
  {:Alias (fn [x] [:Alias {} x [:Printable {} ":"]])
   :Argument (fn [& xs] (reduce (fn [coll x]
                                  (if (= (first x) :Value)
                                    (conj coll [:Printable {} ":"] x)
                                    (conj coll x)))
                                [:Argument {}]
                                xs))
   :Arguments (fn [& xs]
                (reduce
                  (fn [coll x] (conj coll x))
                  [:Arguments {}]
                  (conj (into [[:Printable {} "("]]
                              (interpose [:Printable {} ","] xs))
                        [:Printable {} ")"])))
   :BlockQuote (fn [] "\"\"\"")
   :BlockStringCharacter str
   :BooleanValue boolean-value
   :BraceClose (fn [x] [:Printable {} x])
   :BraceOpen (fn [x] [:Printable {} x])
   :BracketClose (fn [x] [:Printable {} x])
   :BracketOpen (fn [x] [:Printable {} x])
   :Colon (fn [x] [:Printable {} x])
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
   :Directives (fn [& xs]
                 (reduce (fn [coll x] (conj coll x))
                         [:Directives {}]
                         xs))
   :Document (fn [x] [:Document {} x])
   :Equals (fn [x] [:Printable {} x])
   :EscapedCharacter str
   :EscapedUnicode str
   :ExclamationMark (fn [x] [:Printable {} x])
   :ExecutableDefinition (fn [x] [:ExecutableDefinition {} x])
   :ExponentIndicator str
   :ExponentPart str
   :Field (fn [& xs]
            (reduce (fn [coll x] (conj coll x))
                    [:Field {}]
                    xs))
   :FieldDefinition (fn [& xs]
                      (reduce (fn [coll x] (conj coll x))
                              [:FieldDefinition {}]
                              xs))
   :FieldNameSeparator (fn [] [:Printable {} ":"])
   :FieldsDefinition (fn [& xs]
                       (conj (reduce (fn [coll x] (conj coll x))
                                     [:FieldsDefinition {}
                                      [:Printable {} "{"]]
                                     (conj (interpose [:Printable {} " "] xs)))
                             [:Printable {} "}"]))
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
                                   (conj (interpose [:Printable {} "&"] xs))))
   :InlineFragment (fn [& xs]
                     (reduce (fn [coll x] (conj coll x))
                             [:InlineFragment {} [:Printable {} "..."]]
                             xs))
   :IntValue int-value
   :IntegerPart str
   :InterfaceKeyword (fn [x] [:Printable {} x])
   :InterfaceTypeDefinition (fn [& xs]
                              (reduce (fn [coll x] (conj coll x))
                                      [:InterfaceTypeDefinition {}]
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
                           (reduce (fn [coll x]
                                     (if (= (first x) :ImplementsInterfaces)
                                       (conj coll
                                             [:Printable {} "implements"]
                                             [:Printable {} " "]
                                             x)
                                       (conj coll x)))
                                   [:ObjectTypeDefinition {}]
                                   (interpose [:Printable {} " "] xs)))
   :ObjectValue (fn [& xs]
                  (conj (reduce
                          (fn [coll x] (conj coll x))
                          [:ListValue {} [:Printable {} "{"]]
                          (conj (interpose [:Printable {} ","] xs)))
                        [:Printable {} "}"]))
   :OperationDefinition (fn [& xs]
                          (reduce (fn [coll x]
                                    (if (= (first x) :OperationType)
                                      (conj coll x [:Printable {} " "])
                                      (conj coll x)))
                                  [:OperationDefinition {}]
                                  xs))
   :OperationType (fn [x] [:OperationType {} [:Printable {} x]])
   :Quote (fn [] "\"")
   :RootOperationTypeDefinition (fn [& xs]
                                  (reduce (fn [coll x] (conj coll x))
                                          [:RootOperationTypeDefinition {}]
                                          xs))
   :ScalarKeyword (fn [x] [:Printable {} x])
   :ScalarTypeDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:ScalarTypeDefinition {}]
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
                   (conj (reduce
                           (fn [coll x] (conj coll x))
                           [:SelectionSet {} [:Printable {} "{"]]
                           (interpose [:Printable {} ","] xs))
                         [:Printable {} "}"]))
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
   :TypeSystemDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:TypeSystemDefinition {}]
                                   xs))
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

(defn -main [& args]
  (pr-str-ast ""
    (if (first args)
      (document-parser (first args))
      (document-parser
        (str/join "\n" (line-seq (java.io.BufferedReader. *in*)))))))

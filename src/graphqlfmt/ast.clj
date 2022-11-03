(ns graphqlfmt.ast
  (:refer-clojure :exclude [name comment])
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]))

(defn ebnf [& names]
  (apply str (map (fn [name]
                    (-> (str name ".ebnf")
                      io/resource
                      io/reader
                      slurp))
                  names)))

(def document-parser
  (insta/parser (ebnf "document" "token" "ignored")))

(defn comment
  ([] [:Comment {} [:Printable {} "#"]])
  ([x] [:Comment {} [:Printable {} (str "# " x)]])
  ([x & ys] [:Comment {} [:Printable {} (str "# " x (apply str ys))]]))

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

(def transform-map
  {:Alias (fn [& xs]
            (reduce (fn [coll x] (conj coll x))
                    [:Alias {}]
                    xs))
   :Argument (fn [& xs] (reduce (fn [coll x] (conj coll x))
                                [:Argument {}]
                                xs))
   :Arguments (fn [& xs]
                (reduce
                  (fn [coll x] (conj coll x))
                  [:Arguments {}]
                  xs))
   :ArgumentsDefinition (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:ArgumentsDefinition {}]
                                  xs))
   :BlockQuote (fn [] [:BlockQuote {} "\"\"\""])
   :BlockQuoteOpen (fn [] [:BlockQuoteOpen {} [:Printable {} "\"\"\""]])
   :BlockQuoteClose (fn [] [:BlockQuoteClose {} [:Printable {} "\"\"\""]])
   :BlockStringCharacter (fn [s] [:BlockStringCharacter {} s])
   :BlockStringCharacters (fn [& xs]
                            [:BlockStringCharacters {}
                             [:Printable {} (apply str (reduce
                                                         (fn [coll [_node-name _opts s]]
                                                           (conj coll s))
                                                         []
                                                         xs))]])
   :BooleanValue boolean-value
   :BraceClose (fn [x] [:BraceClose {} [:Printable {} x]])
   :BraceOpen (fn [x] [:BraceOpen {} [:Printable {} x]])
   :BracketClose (fn [x] [:Printable {} x])
   :BracketOpen (fn [x] [:Printable {} x])
   :Colon (fn [x] [:Colon {} [:Printable {} x]])
   :Comment comment
   :CommentChar str
   :DefaultValue (fn [& xs]
                   (reduce (fn [coll x] (conj coll x))
                           [:DefaultValue {}]
                           xs))
   :Definition (fn [& xs]
                 (reduce (fn [coll x] (conj coll x))
                         [:Definition {}]
                         xs))
   :Description (fn [x] [:Description {} x])
   :Digit str
   :Directive (fn [& xs]
                (reduce (fn [coll x] (conj coll x))
                        [:Directive {}]
                        (into [[:Printable {} "@"]] xs)))
   :DirectiveDefinition (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:DirectiveDefinition {}]
                                  xs))
   :DirectiveKeyword (fn [x] [:DirectiveKeyword {} [:Printable {} x]])
   :DirectiveLocation (fn [& xs]
                        (reduce (fn [coll x] (conj coll x))
                                [:DirectiveLocation {}]
                                xs))
   :DirectiveLocations (fn [& xs]
                         (reduce (fn [coll x] (conj coll x))
                                 [:DirectiveLocations {}]
                                 xs))
   :DirectivePrefix (fn [x] [:DirectivePrefix {} [:Printable {} x]])
   :Directives (fn [& xs]
                 (reduce (fn [coll x] (conj coll x))
                         [:Directives {}]
                         xs))
   :Document (fn [& xs]
               (reduce (fn [coll x] (conj coll x))
                       [:Document {}]
                       xs))
   :Ellipsis (fn [_] [:Ellipsis {} [:Printable {} "..."]])
   :ExecutableDirectiveLocation (fn [x] [:ExecutableDirectiveLocation {} x])
   :EnumKeyword (fn [x] [:EnumKeyword {} [:Printable {} x]])
   :EnumTypeDefinition (fn [& xs]
                         (reduce (fn [coll x] (conj coll x))
                                 [:EnumTypeDefinition {}]
                                 xs))
   :EnumTypeExtension (fn [& xs]
                        (reduce (fn [coll x] (conj coll x))
                                [:EnumTypeExtension {}]
                                (conj (interpose [:Printable {} " "] xs))))
   :EnumValue (fn [x] [:EnumValue {} [:Printable {} x]])
   :EnumValueDefinition (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:EnumValueDefinition {}]
                                  xs))
   :EnumValuesDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:EnumValuesDefinition {}]
                                   xs))
   :Equals (fn [x] [:Equals {} [:Printable {} x]])
   :EscapedCharacter str
   :EscapedUnicode str
   :ExclamationMark (fn [x] [:Printable {} x])
   :ExecutableDefinition (fn [x] [:ExecutableDefinition {} x])
   :ExponentIndicator str
   :ExponentPart str
   :ExtendKeyword (fn [x] [:ExtendKeyword {} [:Printable {} x]])
   :Field (fn [& xs]
            (reduce (fn [coll x] (conj coll x))
                    [:Field {}]
                    xs))
   :FieldDefinition (fn [& xs]
                      (reduce (fn [coll x] (conj coll x))
                              [:FieldDefinition {}]
                              xs))
   :FieldsDefinition (fn [& xs]
                       (conj (reduce (fn [coll x] (conj coll x))
                                     [:FieldsDefinition {}]
                                     xs)))
   :FloatValue float-value
   :FragmentDefinition (fn [& xs]
                         (reduce (fn [coll x] (conj coll x))
                                 [:FragmentDefinition {}
                                  [:Printable {} "fragment"]
                                  [:Printable {} " "]]
                                 (conj (interpose [:Printable {} " "] xs))))
   :FractionalPart (partial str ".")
   :FragmentName (fn [s] [:FragmentName {} [:Printable {} s]])
   :FragmentSpread (fn [& xs]
                     (reduce (fn [coll x] (conj coll x))
                             [:FragmentSpread {}]
                             xs))
   :ImplementsInterfaces (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:ImplementsInterfaces {}]
                                   xs))
   :ImplementsKeyword (fn [_] [:ImplementsKeyword {} [:Printable {} "implements"]])
   :ImplementsTypeSeparator (fn [_] [:ImplementsTypeSeparator {} [:Printable {} "&"]])
   :InlineFragment (fn [& xs]
                     (reduce (fn [coll x] (conj coll x))
                             [:InlineFragment {}]
                             xs))
   :InputValueDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:InputValueDefinition {}]
                                   xs))
   :InputFieldsDefinition (fn [& xs]
                            (reduce (fn [coll x] (conj coll x))
                                    [:InputFieldsDefinition {}]
                                    xs))
   :InputKeyword (fn [x] [:InputKeyword {} [:Printable {} x]])
   :InputObjectTypeDefinition (fn [& xs]
                                (reduce (fn [coll x] (conj coll x))
                                        [:InputObjectTypeDefinition {}]
                                        xs))
   :InputObjectTypeExtension (fn [& xs]
                               (reduce (fn [coll x] (conj coll x))
                                       [:InputObjectTypeExtension {}]
                                       (conj (interpose [:Printable {} " "] xs))))
   :IntValue int-value
   :IntegerPart str
   :InterfaceKeyword (fn [x] [:InterfaceKeyword {} [:Printable {} x]])
   :InterfaceTypeDefinition (fn [& xs]
                              (reduce (fn [coll x] (conj coll x))
                                      [:InterfaceTypeDefinition {}]
                                      xs))
   :InterfaceTypeExtension (fn [& xs]
                             (reduce (fn [coll x] (conj coll x))
                                     [:InterfaceTypeExtension {}]
                                     (conj (interpose [:Printable {} " "] xs))))
   :LineTerminator (fn [s] [:LineTerminator {} s])
   ;; As per prettier, if multiple LineTerminators are inputted, we only output
   ;; one extra LineTerminator.
   :LineTerminators (fn [_] [:LineTerminator {} "\n"])
   :ListType (fn [& xs]
               (reduce (fn [coll x] (conj coll x))
                       [:ListType {}]
                       xs))
   :ListValue (fn [& xs]
                (reduce (fn [coll x] (conj coll x))
                        [:ListValue {}]
                        xs))
   :Name (fn [x] [:Name {} [:Printable {} x]])
   :NamedType (fn [x] [:NamedType {} x])
   :NegativeSign str
   :NonNullType (fn [& xs]
                  (reduce (fn [coll x] (conj coll x))
                          [:NonNullType {}]
                          xs))
   :NonZeroDigit str
   :NullValue null-value
   :ObjectField (fn [& xs]
                  (reduce (fn [coll x] (conj coll x))
                          [:ObjectField {}]
                          xs))
   :ObjectKeyword (fn [x] [:ObjectKeyword {} [:Printable {} x]])
   :ObjectTypeDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:ObjectTypeDefinition {}]
                                   xs))
   :ObjectTypeExtension (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:ObjectTypeExtension {}]
                                  (interpose [:Printable {} " "] xs)))
   :ObjectValue (fn [& xs]
                  (reduce
                    (fn [coll x]
                      (conj coll x))
                    [:ObjectValue {}]
                    xs))
   :OnKeyword (fn [x] [:OnKeyword {} [:Printable {} x]])
   :OperationDefinition (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:OperationDefinition {}]
                                  xs))
   :OperationType (fn [x] [:OperationType {} [:Printable {} x]])
   :OperationTypeDefinition (fn [& xs]
                              (reduce (fn [coll x] (conj coll x))
                                      [:OperationTypeDefinition {}]
                                      xs))
   :ParensOpen (fn [x] [:ParensOpen {} [:Printable {} x]])
   :ParensClose (fn [x] [:ParensClose {} [:Printable {} x]])
   :PipeCharacter (fn [x] [:PipeCharacter {} [:Printable {} x]])
   :Quote (fn [] [:Quote {} "\""])
   :RootOperationTypeDefinition (fn [& xs]
                                  (reduce (fn [coll x] (conj coll x))
                                          [:RootOperationTypeDefinition {}]
                                          xs))
   :SchemaExtension (fn [& xs]
                      (reduce (fn [coll x] (conj coll x))
                              [:SchemaExtension {}]
                              xs))
   :SchemaKeyword (fn [x] [:SchemaKeyword {} [:Printable {} x]])
   :ScalarKeyword (fn [x] [:ScalarKeyword {} [:Printable {} x]])
   :ScalarTypeDefinition (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:ScalarTypeDefinition {}]
                                   xs))
   :ScalarTypeExtension (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:ScalarTypeExtension {}]
                                  (interpose [:Printable {} " "] xs)))
   :SchemaDefinition (fn [& xs]
                       (reduce (fn [coll x] (conj coll x))
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
   :StringCharacter (fn [s] [:StringCharacter {} s])
   :StringCharacters (fn [& xs]
                       [:StringCharacters {}
                        (apply str (reduce
                                     (fn [coll [_node-name _opts s]]
                                       (conj coll s))
                                     []
                                     xs))])
   :StringValue (fn [& xs]
                  (reduce (fn [coll x] (conj coll x))
                          [:StringValue {}]
                          xs))
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
   :UnionKeyword (fn [x] [:UnionKeyword {} [:Printable {} x]])
   :UnionMemberTypes (fn [& xs]
                       (reduce (fn [coll x] (conj coll x))
                               [:UnionMemberTypes {}]
                               (interpose [:Printable {} " "] xs)))
   :UnionTypeDefinition (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:UnionTypeDefinition {}]
                                  xs))
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
                                        [:VariableDefinitions {}]
                                        xs)
                                [:Printable {} " "]))})

;; public

(defn parse
  [s]
  (->> s
    (document-parser)
    (insta/transform transform-map)))
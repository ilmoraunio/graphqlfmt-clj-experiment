(ns graphql-fmt.core
  (:refer-clojure :exclude [name comment])
  (:require [clojure.java.io :as io]
            [clojure.pprint]
            [clojure.string :as str]
            [instaparse.core :as insta])
  (:gen-class))

;; Noting that our version of `ffirst` should probably learn to ignore comments,
;; since that is well something that may bump up in the ast at that point.

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

(def +indentation-unit+ " ")
(def +indentation-depth+ 2)
(defn indent-s
  [{:keys [indentation-level]}]
  (->> +indentation-unit+
    (repeat (* +indentation-depth+ indentation-level))
    (apply str)))

(defn remove-n-chars
  "Safely removes the first n characters from s."
  [s n]
  (subs s (min (count s) n) (count s)))

(def comma-value
  [:Comma {}
   [:Printable {} ","]])

(defn block-string-value
  [s]
  (let [lines (clojure.string/split s #"[\u000A]|[\u000D](?![\u000A])|[\u000D][\u000A]")
        common-indent (reduce
                        (fn [common-indent line]
                          (let [length (count line)
                                indent (count (re-find #"^[\u0009\u0020]*" line))]
                            (if (and (< indent length)
                                     (or (nil? common-indent)
                                         (< indent common-indent)))
                              indent
                              common-indent)))
                        nil
                        (rest lines))
        lines' (if (some? common-indent)
                 (reduce
                   (fn [acc line]
                     (if (empty? acc)
                       (conj acc line)
                       (conj acc (remove-n-chars line common-indent))))
                   []
                   lines)
                 lines)
        lines'' (reduce
                  (fn [acc line]
                    (if (and (empty? acc)
                             (re-find #"^[\u0009\u0020]*$" line))
                      acc
                      (conj acc line)))
                  []
                  lines')
        lines''' (if-let [last-line (peek lines'')]
                   (if (re-find #"^[\u0009\u0020]*$" last-line)
                     (pop lines'')
                     lines'')
                   lines'')
        formatted (reduce
                    (fn [formatted line] (str formatted "\n" line))
                    (or (first lines''') "")
                    (rest lines'''))]
    formatted))

;; XXX(ilmoraunio): This now complects the forming of an ast and adding
;; representation elements. These two should be split up.

;; XXX(ilmoraunio): Further noting that *all* transformations could be done via
;; `insta/transform`. Observe a transformation done for the ready ast:

(comment (insta/transform
           {:Document (fn [opts & xs]
                        (into [:Document (assoc opts :foo :bar)]
                              xs))}
           [:Document {} [:Definition {} "..."]]))

;; XXX(ilmoraunio): Even further noting that this transform-map should, at first
;; hand, comprise of only the *very basic* ast forming. We can further down the
;; ast pipeline do re-transformation where we amend _additional_ entries to the
;; ast. But not before we have initialized the `[node options sub-tree]` format
;; via `ast` (with exception to leaf values that make sense to initialize here,
;; leaves are part of the ast too).

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
                            ;; XXX(ilmoraunio): I wonder if this should be done
                            ;;                  in the transform phase.
                            [:BlockStringCharacters {}
                             [:Printable {} (apply str (reduce
                                                         (fn [coll [_node-name _opts s]]
                                                           (conj coll s))
                                                         []
                                                         xs))]])
   :BooleanValue boolean-value
   :BraceClose (fn [x] [:BraceClose {} [:Printable {} x]])
   ;; XXX(ilmoraunio): Okay, it seems this is now the exception wherein a
   ;; leaf node type now is wrapped within a Printable, making it stand out of
   ;; all the leaf node types which are at this point *not* wrapped. I think
   ;; this ought to be resolved somehow at some point, IMO this will not be
   ;; tenable in the long run.
   :BraceOpen (fn [x] [:BraceOpen {} [:Printable {} x]])
   :BracketClose (fn [x] [:Printable {} x])
   :BracketOpen (fn [x] [:Printable {} x])
   ;; XXX(ilmoraunio): Ok, it seems we are having to do wrap-overs to some
   ;; leaf element as we move forward with formatting. That's actually OK. Let's
   ;; see how far this takes us.
   :Colon (fn [x] [:Colon {} [:Printable {} x]])
   :Commas (fn
             ([] comma-value)
             ([_] comma-value))
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

(defn validate
  [ast]
  (let [[node opts & rst] ast]
    (when (and (not (vector? (first rst)))
               (not (string? (first rst))))
      (throw (ex-info {} (str "Unrecognized type: " (type (first rst))))))
    (into [node opts]
          (cond
            (vector? (first rst)) (map validate rst)
            (string? (first rst)) rst))))

;; enrich ast opts fns

(defn amend-indentation-level-opts
  [indent-level ast]
  (let [[node opts & rst] ast]
    (let [indent-level (case node
                         (:Arguments
                           :EnumValuesDefinition
                           :FieldsDefinition
                           :InputFieldsDefinition
                           :OperationTypeDefinition
                           :RootOperationTypeDefinition
                           :SelectionSet) (inc indent-level)
                         (:ParensClose
                           :BraceClose) (max (dec indent-level) 0)
                         indent-level)]
      (into [node (into opts {:indentation-level indent-level})]
            (cond
              (vector? (first rst)) (map (partial amend-indentation-level-opts indent-level) rst)
              (string? (first rst)) rst)))))

(defn amend-newline-opts
  [ast]
  (let [m {:Comment (fn [opts & xs]
                      (into [:Comment (assoc opts :append-newline? true)]
                            xs))
           :Description (fn [opts & xs]
                          (into [:Description (assoc opts :append-newline? true
                                                          :prepend-indent? true)]
                                xs))
           :Document (fn [opts & xs]
                       (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                       (-> (update acc-head :acc conj
                                                   (if (and (= node :Definition)
                                                            (= (ffirst head) :Definition))
                                                     (into [node (assoc opts :append-newline? true
                                                                             :prepend-indent? true)]
                                                           rst)
                                                     x))
                                         (update :head rest)))
                                     {:acc [:Document opts]
                                      :head (rest xs)}
                                     xs)))
           :BraceClose (fn [opts & xs]
                         (into [:BraceClose (assoc opts :prepend-indent? true)]
                               xs))
           :EnumValueDefinition (fn [opts & xs]
                                  (reduce (fn [coll [node opts & rst]]
                                            (conj coll
                                                  (into [node
                                                         (cond-> opts
                                                           (= node :EnumValue) (assoc :prepend-indent? true))]
                                                        rst)))
                                          [:EnumValueDefinition opts]
                                          xs))
           :EnumValuesDefinition (fn [opts & xs]
                                   (reduce (fn [coll [node opts & rst]]
                                             (conj coll
                                                   (into [node
                                                          (cond-> opts
                                                            (= node :BraceOpen) (assoc :append-newline? true)
                                                            (= node :EnumValueDefinition) (assoc :append-newline? true))]
                                                         rst)))
                                           [:EnumValuesDefinition opts]
                                           xs))
           :FieldDefinition (fn [opts & xs]
                              (reduce (fn [coll [node opts & rst]]
                                        (conj coll
                                              (into [node
                                                     (cond-> opts
                                                       (= node :Name) (assoc :prepend-indent? true))]
                                                    rst)))
                                      [:FieldDefinition opts]
                                      xs))
           :FieldsDefinition (fn [opts & xs]
                               (reduce (fn [coll [node opts & rst]]
                                         (conj coll
                                               (into [node
                                                      (cond-> opts
                                                        (= node :BraceOpen) (assoc :append-newline? true)
                                                        (= node :FieldDefinition) (assoc :append-newline? true))]
                                                     rst)))
                                       [:FieldsDefinition opts]
                                       xs))
           :InputFieldsDefinition (fn [opts & xs]
                                    (reduce (fn [coll [node opts & rst :as x]]
                                              (conj coll
                                                    (into [node
                                                           (cond-> opts
                                                             (= node :BraceOpen) (assoc :append-newline? true)
                                                             (= node :InputValueDefinition) (assoc :append-newline? true))]
                                                          rst)))
                                            [:InputFieldsDefinition opts]
                                            xs))
           :InputValueDefinition (fn [opts & xs]
                                   (reduce (fn [coll [node opts & rst]]
                                             (conj coll
                                                   (into [node
                                                          (cond-> opts
                                                            (= node :Name) (assoc :prepend-indent? true))]
                                                         rst)))
                                           [:InputValueDefinition opts]
                                           xs))
           :SchemaDefinition (fn [opts & xs]
                               (reduce (fn [coll [node opts & rst :as _x]]
                                         (conj coll
                                               (into [node
                                                      (cond-> opts
                                                        (= node :BraceOpen) (assoc :append-newline? true)
                                                        (= node :RootOperationTypeDefinition) (assoc :prepend-indent? true
                                                                                                     :append-newline? true))]
                                                     rst)))
                                       [:SchemaDefinition opts]
                                       xs))
           :SchemaExtension (fn [opts & xs]
                              (reduce (fn [coll [node opts & rst :as _x]]
                                        (conj coll
                                              (into [node
                                                     (cond-> opts
                                                       (= node :BraceOpen) (assoc :append-newline? true)
                                                       (= node :OperationTypeDefinition) (assoc :prepend-indent? true
                                                                                                :append-newline? true))]
                                                    rst)))
                                      [:SchemaExtension opts]
                                      xs))
           :SelectionSet (fn [opts & xs]
                           (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as _x]]
                                           (-> (update acc-head :acc conj
                                                       (into [node
                                                              (cond-> opts
                                                                (= node :BraceOpen) (assoc :append-newline? true)
                                                                (and (= node :Selection)
                                                                     (not= (ffirst head) :BraceClose)) (assoc :prepend-indent? true
                                                                                                              :append-newline? true)
                                                                (and (= node :Selection)
                                                                     (= (ffirst head) :BraceClose)) (assoc :prepend-indent? true
                                                                                                           :append-newline? true))]
                                                             rst))
                                             (update :head rest)))
                                         {:acc [:SelectionSet opts]
                                          :head (rest xs)}
                                         xs)))
           :StringValue (fn [opts & xs]
                          (reduce (fn [coll [node opts & rst]]
                                    (conj coll
                                          (into [node
                                                 (cond-> opts
                                                   (= node :BlockQuoteOpen) (assoc :append-newline? true)
                                                   (= node :BlockStringCharacters) (assoc :append-newline? true)
                                                   (= node :BlockQuoteClose) (assoc :prepend-indent? true))]
                                                rst)))
                                  [:StringValue opts]
                                  xs))}]
    (insta/transform m ast)))

(defn amend-horizontal-spacing-opts
  [ast]
  (let [m {:Arguments (fn [opts & xs]
                        (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                        (-> (update acc-head :acc conj
                                                    (if (and (= node :Argument)
                                                             (= (ffirst head) :Argument))
                                                      (into [node (assoc opts :append-whitespace? true)] rst)
                                                      x))
                                          (update :head rest)))
                                      {:acc [:Arguments opts]
                                       :head (rest xs)}
                                      xs)))
           :ArgumentsDefinition (fn [opts & xs]
                                  (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                  (-> (update acc-head :acc conj
                                                              (if (and (= node :InputValueDefinition)
                                                                       (= (ffirst head) :InputValueDefinition))
                                                                (into [node (assoc opts :append-whitespace? true)] rst)
                                                                x))
                                                    (update :head rest)))
                                                {:acc [:ArgumentsDefinition opts]
                                                 :head (rest xs)}
                                                xs)))
           :DefaultValue (fn [opts & xs]
                           (reduce (fn [coll [node opts & rst :as x]]
                                     (conj coll
                                           (if (= node :Equals)
                                             (into [node (assoc opts :append-whitespace? true)] rst)
                                             x)))
                                   [:DefaultValue opts]
                                   xs))
           :DirectiveDefinition (fn [opts & xs]
                                  (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                  (-> (update acc-head :acc conj
                                                              (if (or (#{:ArgumentsDefinition
                                                                         :DirectiveKeyword
                                                                         :OnKeyword} node)
                                                                      (and (= node :Name)
                                                                           (= (ffirst head) :OnKeyword)))
                                                                (into [node (assoc opts :append-whitespace? true)] rst)
                                                                x))
                                                    (update :head rest)))
                                                {:acc [:DirectiveDefinition opts]
                                                 :head (rest xs)}
                                                xs)))
           :DirectiveLocations (fn [opts & xs]
                                 (reduce (fn [coll [node opts & rst :as x]]
                                           (conj coll
                                                 (if (#{:DirectiveLocations
                                                        :PipeCharacter} node)
                                                   (into [node (assoc opts :append-whitespace? true)] rst)
                                                   x)))
                                         [:DirectiveLocations opts]
                                         xs))
           :Directives (fn [opts & xs]
                         (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                         (-> (update acc-head :acc conj
                                                     (if (and (= node :Directive)
                                                              (= (ffirst head) :Directive))
                                                       (into [node (assoc opts :append-whitespace? true)] rst)
                                                       x))
                                           (update :head rest)))
                                       {:acc [:Directives opts]
                                        :head (rest xs)}
                                       xs)))
           :EnumTypeDefinition (fn [opts & xs]
                                 (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                 (-> (update acc-head :acc conj
                                                             (if (or (= node :EnumKeyword)
                                                                     (and (= node :Name)
                                                                          (#{:Directives
                                                                             :EnumValuesDefinition} (ffirst head)))
                                                                     (and (= node :Directives)
                                                                          (= (ffirst head) :EnumValuesDefinition)))
                                                               (into [node (assoc opts :append-whitespace? true)] rst)
                                                               x))
                                                   (update :head rest)))
                                               {:acc [:EnumTypeDefinition opts]
                                                :head (rest xs)}
                                               xs)))
           :EnumValueDefinition (fn [opts & xs]
                                  (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                  (-> (update acc-head :acc conj
                                                              (if (and (= node :EnumValue)
                                                                       (= (ffirst head) :Directives))
                                                                (into [node (assoc opts :append-whitespace? true)] rst)
                                                                x))
                                                    (update :head rest)))
                                                {:acc [:EnumValueDefinition opts]
                                                 :head (rest xs)}
                                                xs)))
           :Field (fn [opts & xs]
                    (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                    (-> (update acc-head :acc conj
                                                (if (and (= node :Name)
                                                         (#{:Directives :SelectionSet} (ffirst head)))
                                                  (into [node (assoc opts :append-whitespace? true)] rst)
                                                  x))
                                      (update :head rest)))
                                  {:acc [:Field opts]
                                   :head (rest xs)}
                                  xs)))
           :FieldDefinition (fn [opts & xs]
                              (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                              (-> (update acc-head :acc conj
                                                          (if (and (= node :Type)
                                                                   (= (ffirst head) :Directives))
                                                            (into [node (assoc opts :append-whitespace? true)] rst)
                                                            x))
                                                (update :head rest)))
                                            {:acc [:FieldDefinition opts]
                                             :head (rest xs)}
                                            xs)))
           :FragmentSpread (fn [opts & xs]
                             (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                             (-> (update acc-head :acc conj
                                                         (if (and (= node :FragmentName)
                                                                  (#{:Directives :TypeCondition} (ffirst head)))
                                                           (into [node (assoc opts :append-whitespace? true)] rst)
                                                           x))
                                               (update :head rest)))
                                           {:acc [:FragmentName opts]
                                            :head (rest xs)}
                                           xs)))
           :InlineFragment (fn [opts & xs]
                             (reduce (fn [coll [node opts & rst :as x]]
                                       (conj coll
                                             (if (#{:Ellipsis
                                                    :TypeCondition
                                                    :Directives} node)
                                               (into [node (assoc opts :append-whitespace? true)] rst)
                                               x)))
                                     [:InlineFragment opts]
                                     xs))
           :ImplementsInterfaces (fn [opts & xs]
                                   (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                   (-> (update acc-head :acc conj
                                                               (if (or (#{:ImplementsKeyword
                                                                          :ImplementsTypeSeparator} node)
                                                                       (and (= node :ImplementsInterfaces)
                                                                            (= (ffirst head) :ImplementsTypeSeparator)))
                                                                 (into [node (assoc opts :append-whitespace? true)] rst)
                                                                 x))
                                                     (update :head rest)))
                                                 {:acc [:ImplementsInterfaces opts]
                                                  :head (rest xs)}
                                                 xs)))
           :InputObjectTypeDefinition (fn [opts & xs]
                                        (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                        (-> (update acc-head :acc conj
                                                                    (if (or (= node :InputKeyword)
                                                                            (and (= node :Name)
                                                                                 (#{:Directives
                                                                                    :InputFieldsDefinition} (ffirst head)))
                                                                            (and (= node :Directives)
                                                                                 (= (ffirst head) :InputFieldsDefinition)))
                                                                      (into [node (assoc opts :append-whitespace? true)] rst)
                                                                      x))
                                                          (update :head rest)))
                                                      {:acc [:InputObjectTypeDefinition opts]
                                                       :head (rest xs)}
                                                      xs)))
           :InputValueDefinition (fn [opts & xs]
                                   (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                   (-> (update acc-head :acc conj
                                                               (if (or (and (= node :Type)
                                                                            (#{:DefaultValue
                                                                               :Directives} (ffirst head)))
                                                                       (and (= node :DefaultValue)
                                                                            (= (ffirst head) :Directives)))
                                                                 (into [node (assoc opts :append-whitespace? true)] rst)
                                                                 x))
                                                     (update :head rest)))
                                                 {:acc [:InputValueDefinition opts]
                                                  :head (rest xs)}
                                                 xs)))
           :InterfaceTypeDefinition (fn [opts & xs]
                                      (:acc (reduce (fn [{:keys [_head] :as acc-head} [node opts & rst :as x]]
                                                      (-> (update acc-head :acc conj
                                                                  (if (#{:InterfaceKeyword
                                                                         :Name
                                                                         :Directives} node)
                                                                    (into [node (assoc opts :append-whitespace? true)] rst)
                                                                    x))
                                                        (update :head rest)))
                                                    {:acc [:InterfaceTypeDefinition opts]
                                                     :head (rest xs)}
                                                    xs
                                                    )))
           :ListValue (fn [opts & xs]
                        (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                        (-> (update acc-head :acc conj
                                                    (if (and (= node :Value)
                                                             (= (ffirst head) :Value))
                                                      (into [node (assoc opts :append-whitespace? true)] rst)
                                                      x))
                                          (update :head rest)))
                                      {:acc [:ListValue opts]
                                       :head (rest xs)}
                                      xs)))
           :Colon (fn [opts & xs]
                    (into [:Colon (assoc opts :append-whitespace? true)]
                          xs))
           :ObjectField (fn [opts & xs]
                          (into [:ObjectField (assoc opts :append-whitespace?
                                                          true)]
                                xs))
           :OperationDefinition (fn [opts & xs]
                                  (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                  (-> (update acc-head :acc conj
                                                              (if (or (#{:Directives
                                                                         :OperationType} node)
                                                                      (and (= node :Name)
                                                                           (#{:Directives
                                                                              :SelectionSet} (ffirst head))))
                                                                (into [node (assoc opts :append-whitespace? true)] rst)
                                                                x))
                                                    (update :head rest)))
                                                {:acc [:OperationDefinition opts]
                                                 :head (rest xs)}
                                                xs)))
           :ObjectTypeDefinition (fn [opts & xs]
                                   (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                   (-> (update acc-head :acc conj
                                                               (if (or (#{:ObjectKeyword} node)
                                                                       (and (= node :ImplementsInterfaces)
                                                                            (#{:Directives
                                                                               :FieldsDefinition} (ffirst head)))
                                                                       (and (= node :Name)
                                                                            (#{:ImplementsInterfaces
                                                                               :Directives
                                                                               :FieldsDefinition} (ffirst head))))
                                                                 (into [node (assoc opts :append-whitespace? true)]
                                                                       rst)
                                                                 x))
                                                     (update :head rest)))
                                                 {:acc [:ObjectTypeDefinition opts]
                                                  :head (rest xs)}
                                                 xs)))
           :ObjectValue (fn [opts & xs]
                          (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                          (-> (update acc-head :acc conj
                                                      (if (and (= node :BraceOpen)
                                                               (= (ffirst head) :ObjectField))
                                                        (into [node (assoc opts :append-whitespace? true)]
                                                              rst)
                                                        x))
                                            (update :head rest)))
                                        {:acc [:ObjectValue opts]
                                         :head (rest xs)}
                                        xs)))
           :ScalarTypeDefinition (fn [opts & xs]
                                   (reduce (fn [coll [node opts & rst :as x]]
                                             (conj coll
                                                   (if (#{:ScalarKeyword
                                                          :Name} node)
                                                     (into [node (assoc opts :append-whitespace? true)]
                                                           rst)
                                                     x)))
                                           [:ScalarTypeDefinition opts]
                                           xs))
           :SchemaDefinition (fn [opts & xs]
                               (reduce (fn [coll [node opts & rst :as x]]
                                         (conj coll
                                               (if (= node :Directives)
                                                 (into [node (assoc opts :append-whitespace? true)] rst)
                                                 x)))
                                       [:SchemaDefinition opts]
                                       xs))
           :SchemaExtension (fn [opts & xs]
                              (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                              (-> (update acc-head :acc conj
                                                          (if (or (#{:ExtendKeyword
                                                                     :SchemaKeyword} node)
                                                                  (and (= node :Directives)
                                                                       (= (ffirst head) :BraceOpen)))
                                                            (into [node (assoc opts
                                                                          :append-whitespace?
                                                                          true)]
                                                                  rst)
                                                            x))
                                                (update :head rest)))
                                            {:acc [:SchemaExtension opts]
                                             :head (rest xs)}
                                            xs)))
           :UnionTypeDefinition (fn [opts & xs]
                                  (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                  (-> (update acc-head :acc conj
                                                              (if (or (= node :UnionKeyword)
                                                                      (and (= node :Name)
                                                                           (#{:Directives
                                                                              :UnionMemberTypes} (ffirst head)))
                                                                      (and (= node :Directives)
                                                                           (= (ffirst head) :UnionMemberTypes)))
                                                                (into [node (assoc
                                                                              opts
                                                                              :append-whitespace?
                                                                              true)]
                                                                      rst)
                                                                x))
                                                    (update :head rest)))
                                                {:acc [:UnionTypeDefinition opts]
                                                 :head (rest xs)}
                                                xs)))
           :VariableDefinition (fn [opts & xs]
                                 (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                 (-> (update acc-head :acc conj
                                                             (if (and (= node :Type)
                                                                      (= (ffirst head) :DefaultValue))
                                                               (into [node (assoc opts :append-whitespace? true)]
                                                                     rst)
                                                               x))
                                                   (update :head rest)))
                                               {:acc [:VariableDefinition opts]
                                                :head (rest xs)}
                                               xs)))
           :VariableDefinitions (fn [opts & xs]
                                  (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                                  (-> (update acc-head :acc conj
                                                              (if (and (= node :VariableDefinition)
                                                                       (= (ffirst head) :VariableDefinition))
                                                                (into [node (assoc opts :append-whitespace? true)]
                                                                      rst)
                                                                x))
                                                    (update :head rest)))
                                                {:acc [:VariableDefinitions opts]
                                                 :head (rest xs)}
                                                xs)))}]
    (insta/transform m ast)))

(defn amend-prefer-inlining-opts
  [ast]
  (let [m {:ArgumentsDefinition (fn [opts & xs]
                                  (reduce (fn [coll [node opts & rst :as x]]
                                            (conj coll
                                                  (if (= node :InputValueDefinition)
                                                    (reduce (fn [coll [node opts & rst]]
                                                              (conj coll
                                                                    (into [node
                                                                           (cond-> opts
                                                                             (= node :Description) (assoc :prefer-inlining? true
                                                                                                          :append-whitespace? true)
                                                                             (= node :Name) (assoc :prefer-inlining? true))]
                                                                          rst)))
                                                            [:InputValueDefinition opts]
                                                            rst)
                                                    x)))
                                          [:ArgumentsDefinition opts]
                                          xs))
           :ObjectValue (fn [opts & xs]
                          (reduce (fn [coll [node opts & rst :as x]]
                                    (conj coll
                                          (if (= node :BraceClose)
                                            (into [node (assoc opts
                                                          :prefer-inlining?
                                                          true)]
                                                  rst)
                                            x)))
                                  [:ObjectValue opts]
                                  xs))}]
    (insta/transform m ast)))

;; enrich-ast-opts fns

(defn amend-horizontal-spacing
  [ast]
  (let [[node opts & rst] ast]
    (into [node opts]
          (cond
            (vector? (first rst)) (map amend-horizontal-spacing
                                       (if (:append-whitespace? opts)
                                         (concat rst [[:Printable {} " "]])
                                         rst))
            ;; XXX(ilmoraunio): One could form an opinion that we should
            ;; transform all our node types to produce Printable nodes so that
            ;; we can support `append-whitespace?` for all node types. This
            ;; could indeed be done, but it's not something that is yet forcing
            ;; my hand, thus deferring.
            (string? (first rst)) rst))))

(defn amend-structured-tree-opts
  [ast]
  (letfn [(check [[node _opts & rst]]
            (cond
              (= :BlockStringCharacters node) true
              (vector? (first rst)) (reduce
                                      (fn [_ node]
                                        (when (check node)
                                          (reduced true)))
                                      false
                                      rst)
              :else false))]
    (let [[node opts & rst] ast]
      (into [node (cond-> opts
                    (and (= node :Field)
                         (check ast)) (assoc :structured-tree? true))]
            (cond
              (vector? (first rst)) (map amend-structured-tree-opts rst)
              (string? (first rst)) rst)))))

(defn amend-newline-to-structure-tree-opts
  ([ast]
   (let [[_node {:keys [structured-tree?] :as _opts} & _rst] ast]
     (amend-newline-to-structure-tree-opts structured-tree? ast)))
  ([structured? ast]
   (let [[node {:keys [structured-tree?] :as opts} & rst] ast
         within-structured-subtree? (or structured? structured-tree?)]
     (into [node (cond-> opts
                   (and within-structured-subtree?
                        (#{:Argument
                           :ObjectField} node)) (assoc :prepend-newline? true
                                                       :prepend-indent? true)
                   (and within-structured-subtree?
                        (#{:ParensClose} node)) (assoc :prepend-indent? true)
                   (and within-structured-subtree?
                        (#{:BlockQuoteClose} node)) (assoc :prepend-indent? true
                                                           :append-newline? true)
                   (and within-structured-subtree?
                        (= node :BlockStringCharacters)) (assoc :append-newline? true))]
           (cond
             (vector?
               (first rst)) (map (partial amend-newline-to-structure-tree-opts
                                          (or structured? structured-tree?))
                                 rst)
             (string? (first rst)) rst)))))

(defn ast
  [s]
  (->> s
    document-parser
    (insta/transform transform-map)))

(defn transform
  [ast]
  (let [m {:Arguments (fn [opts & xs]
                        (:acc (reduce (fn [{:keys [head] :as acc-head} [node & _ :as x]]
                                        (-> (update acc-head :acc conj
                                                    (if (and (= node :Argument)
                                                             (= (ffirst head) :Argument))
                                                      (into x [comma-value])
                                                      x))
                                          (update :head rest)))
                                      {:acc [:Arguments opts]
                                       :head (rest xs)}
                                      xs)))
           :ArgumentsDefinition (fn [opts & xs]
                                  (:acc (reduce (fn [{:keys [head] :as acc-head} [node & _ :as x]]
                                                  (-> (update acc-head :acc conj
                                                              (if (and (= node :InputValueDefinition)
                                                                       (= (ffirst head) :InputValueDefinition))
                                                                (into x [comma-value])
                                                                x))
                                                    (update :head rest)))
                                                {:acc [:ArgumentsDefinition opts]
                                                 :head (rest xs)}
                                                xs)))
           :ListValue (fn [opts & xs]
                        (conj (:acc (reduce (fn [{:keys [head] :as acc-head} [node & _ :as x]]
                                              (-> (update acc-head :acc conj
                                                          (if (and (= node :Value)
                                                                   (= (ffirst head) :Value))
                                                            (into x [comma-value])
                                                            x))
                                                (update :head rest)))
                                            {:acc [:ListValue opts [:Printable {} "["]]
                                             :head (rest xs)}
                                            xs))
                              [:Printable {} "]"]))
           :ObjectValue (fn [opts & xs]
                          (:acc (reduce (fn [{:keys [head] :as acc-head} [node & _ :as x]]
                                          (-> (update acc-head :acc conj
                                                      (if (and (= node :ObjectField)
                                                               (= (ffirst head) :ObjectField))
                                                        (into x [comma-value])
                                                        x))
                                            (update :head rest)))
                                        {:acc [:ObjectValue opts]
                                         :head (rest xs)}
                                        xs)))
           :VariableDefinitions (fn [opts & xs]
                                  (:acc (reduce (fn [{:keys [head] :as acc-head} [node & _ :as x]]
                                                  (-> (update acc-head :acc conj
                                                              (if (and (= node :VariableDefinition)
                                                                       (= (ffirst head) :VariableDefinition))
                                                                (into x [comma-value])
                                                                x))
                                                    (update :head rest)))
                                                {:acc [:VariableDefinitions opts]
                                                 :head (rest xs)}
                                                xs)))}]
    (insta/transform m ast)))

(defn opts
  [ast]
  (->> ast
    amend-newline-opts
    (amend-indentation-level-opts 0)
    amend-horizontal-spacing-opts
    amend-structured-tree-opts
    amend-newline-to-structure-tree-opts
    amend-prefer-inlining-opts))

(defn amend-characters-opts
  [ast]
  (letfn [(characters [[_node _opts & rst]]
            (cond
              (nil? (first rst)) 0
              (vector? (first rst)) (reduce + (map characters rst))
              (string? (first rst)) (or (and
                                          ;; By convention, we expect rogue newlines
                                          ;; not to show up in the middle of a
                                          ;; subtree.
                                          (not= (first rst) "\n")
                                          (count (first rst)))
                                        0)))]
    (let [[node opts & rst] ast]
      (into [node (assoc opts :character-count (characters ast))]
            (cond
              (vector? (first rst)) (map amend-characters-opts rst)
              (string? (first rst)) rst)))))

(defn re-opts
  [ast]
  (->> ast
    (amend-characters-opts)))

(defn amend-newline-spacing
  [ast]
  (let [[node opts & rst] ast]
    (into (if (not (:prefer-inlining? opts))
            (cond-> [node opts]
              (:prepend-newline? opts) (conj [:Printable {} "\n"])
              (:prepend-indent? opts) (conj [:Printable {} (indent-s opts)]))
            [node opts])
          (cond
            (vector? (first rst)) (map amend-newline-spacing
                                       (cond-> rst
                                         (and (not (:prefer-inlining? opts))
                                              (:append-newline? opts)) (concat [[:Printable {} "\n"]])))
            (string? (first rst)) (if (:newline? opts)
                                    [[:Printable {} (first rst)]]
                                    rst)))))

;; re-transformation fns

(defn block-string-characters
  [s opts]
  (letfn [(empty-line? [line] (re-find #"^[\u0009\u0020]*$" line))]
    (let [lines (clojure.string/split (block-string-value s) #"\n")]
      (reduce
        (fn [formatted line]
          (if (empty-line? line)
            (str formatted "\n" line)
            (str formatted "\n" (indent-s opts) line)))
        (or (if (empty-line? (first lines))
              (str (first lines))
              (str (indent-s opts) (first lines)))
            "")
        (rest lines)))))

(defn format-block-string-values
  [ast]
  (let [m {:BlockStringCharacters (fn [opts [_ printable-opts s] & xs]
                                    (into [:BlockStringCharacters opts
                                           [:Printable {} (block-string-characters s printable-opts)]]
                                          xs))}]
    (insta/transform m ast)))

(defn re-transform
  [ast]
  (-> ast
    (format-block-string-values)
    ;; TODO: remove all Comma elements from under structured-tree? (except from under ListValues)
    (amend-newline-spacing)
    (amend-horizontal-spacing)))

(defn xf
 [s]
  (->> s
    ast
    transform
    validate
    opts
    re-transform
    re-opts))

(defn row-transform
  [ast]
  ;; start going through the ast stack recursively
  ;; if the node's value is not a newline, add to the current row
  ;; if the node's value is a newline, add value to current row and add a new row
  ;; if the current branch is exhausted, return current rows and pass it to the next branch recursion
  ;; go through all the branches until the stack is completely exhausted
  ;; build a rows ast from the collection of rows
  )

(defn row-xf
  [ast]
  (->> ast
    row-transform))

(defn pr-s
  [ast]
  (->> ast
    (pr-str-ast "")
    (clojure.core/format "%s\n")))

(defn fmt
  [s]
  (->> s
    xf
    row-xf
    pr-s))

(defn -main [& args]
  (let [graphql (if (>= (count args) 1)
                  (slurp (first args))
                  (slurp *in*))
        output (fmt graphql)]
    (print output)
    (flush)))

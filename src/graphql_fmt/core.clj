(ns graphql-fmt.core
  (:refer-clojure :exclude [name comment])
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
                                  (interpose [:Printable {} " "] xs)))
   :BlockQuote (fn [] [:BlockQuote {} "\"\"\""])
   :BlockQuoteOpen (fn [] [:BlockQuoteOpen {} "\"\"\""])
   :BlockQuoteClose (fn [] [:BlockQuoteClose {} "\"\"\""])
   :BlockStringCharacter (fn [s] [:BlockStringCharacter {} s])
   :BlockStringCharacters (fn [& xs]
                            [:BlockStringCharacters {}
                             (apply str (reduce
                                          (fn [coll [_node-name _opts s]]
                                            ;; XXX(ilmoraunio): Guaranteed to
                                            ;; be BlockStringCharacter at this
                                            ;; point.
                                            (conj coll s))
                                          []
                                          xs))])
   :BooleanValue boolean-value
   :BraceClose (fn [x] [:BraceClose {} x])
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
   :Ellipsis (fn [_] [:Ellipsis {} [:Printable {} "..."]])
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
            (reduce (fn [coll x] (conj coll x))
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
   :FragmentName (fn [s] [:FragmentName {} [:Printable {} s]])
   :FragmentSpread (fn [& xs]
                     (reduce (fn [coll x] (conj coll x))
                             [:FragmentSpread {}]
                             xs))
   :ImplementsInterfaces (fn [& xs]
                           (reduce (fn [coll x] (conj coll x))
                                   [:ImplementsInterfaces {}]
                                   (conj (interpose [:Printable {} " "] xs))))
   :ImplementsKeyword (fn [x] [:Printable {} x])
   :ImplementsTypeSeparator (fn [x] [:Printable {} x])
   :InlineFragment (fn [& xs]
                     (reduce (fn [coll x] (conj coll x))
                             [:InlineFragment {}]
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
                  (reduce
                    (fn [coll x]
                      (conj coll x))
                    [:ObjectValue {}]
                    xs))
   :OnKeyword (fn [x] [:Printable {} x])
   :OperationDefinition (fn [& xs]
                          (reduce (fn [coll x] (conj coll x))
                                  [:OperationDefinition {}]
                                  xs))
   :OperationType (fn [x] [:OperationType {} [:Printable {} x]])
   :OperationTypeDefinition (fn [& xs]
                              (reduce (fn [coll x] (conj coll x))
                                      [:OperationTypeDefinition {}]
                                      (interpose [:Printable {} " "] xs)))
   :ParensOpen (fn [x] [:ParensOpen {} x])
   :ParensClose (fn [x] [:ParensClose {} x])
   :PipeCharacter (fn [x] [:Printable {} x])
   :Quote (fn [] [:Quote {} "\""])
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
                                   (conj coll comma-value x)
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
                         (:SelectionSet :Arguments :Value) (inc indent-level)
                         (:ParensClose :BraceClose :BlockQuoteClose :BlockStringCharacters) (max (dec indent-level) 0)
                         indent-level)]
      (into [node (into opts {:indentation-level indent-level})]
            (cond
              (vector? (first rst)) (map (partial amend-indentation-level-opts indent-level) rst)
              (string? (first rst)) rst)))))

(defn amend-newline-opts
  [ast]
  (let [[node opts & rst] ast]
    (into [node (cond-> opts
                  (#{:BraceClose
                     :Selection} node) (assoc :newline? true :indent? true))]
          (cond
            (vector? (first rst)) (map amend-newline-opts rst)
            (string? (first rst)) rst))))

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
  (let [[node opts & rst] ast]
    (into [node opts]
          (cond
            (vector? (first rst)) (map amend-prefer-inlining-opts
                                       (if (#{:ObjectValue} node)
                                         (let [head-of-rst (butlast rst)
                                               [node opts & rst] (last rst)]
                                           (concat head-of-rst
                                                   [(into [node
                                                           (assoc
                                                             opts
                                                             :prefer-inlining?
                                                             true)]
                                                          rst)]))
                                         rst))
            (string? (first rst)) rst))))

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
                           :BlockQuoteClose
                           :ObjectField
                           :ParensClose} node)) (assoc :newline? true
                                                       :indent? true)
                   (and within-structured-subtree?
                        (= node :BlockStringCharacters)) (assoc :newline? true))]
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

(defn amend-newline-spacing
  [ast]
  (let [[node opts & rst] ast]
    (into (if (and (not (:prefer-inlining? opts))
                   (:newline? opts))
            (into [node opts [:Printable {} "\n"]]
                  (cond-> []
                    (:indent? opts) (conj [:Printable {} (indent-s opts)])))
            [node opts])
          (cond
            (vector? (first rst)) (map amend-newline-spacing rst)
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
  (let [[node opts & rst] ast]
    (into [node opts]
          (cond
            (vector? (first rst)) (map format-block-string-values rst)
            (string? (first rst)) (case node
                                    :BlockStringCharacters [(block-string-characters
                                                              (first rst)
                                                              opts)]
                                    rst)))))

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
    re-transform))

(defn fmt
  [s]
  (->> s
    xf
    (pr-str-ast "")
    (clojure.core/format "%s\n")))

;; TODO: Figure out why multi-line block string values are not printed out correctly in CLI.
(defn -main [& args]
  (let [graphql (or (first args) (apply str (line-seq (java.io.BufferedReader. *in*))))
        output (fmt graphql)]
    (print output)
    (flush)))

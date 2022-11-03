(ns graphqlfmt.options
  (:require [instaparse.core :as insta]))

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

(defn amend-indentation-level-opts
  ([ast]
   (amend-indentation-level-opts 0 ast))
  ([indent-level ast]
   (let [[node opts & rst] ast]
     (let [indent-level (case node
                          (:Arguments
                            :EnumValuesDefinition
                            :FieldsDefinition
                            :InputFieldsDefinition
                            :OperationTypeDefinition
                            :RootOperationTypeDefinition
                            :SelectionSet
                            :VariableDefinitions) (inc indent-level)
                          (:ParensClose
                            :BraceClose) (max (dec indent-level) 0)
                          indent-level)]
       (into [node (into opts {:indentation-level indent-level})]
             (cond
               (vector? (first rst)) (map (partial amend-indentation-level-opts indent-level) rst)
               (string? (first rst)) rst))))))

(defn amend-horizontal-spacing-opts
  [ast]
  (let [m {:Arguments (fn [opts & xs]
                        (:acc (reduce (fn [{:keys [head] :as acc-head} [node opts & rst :as x]]
                                        (-> (update acc-head :acc conj
                                                    (if (and (= node :Argument)
                                                             (= (ffirst head) :Argument))
                                                      (into [node (assoc opts :append-softspace? true)] rst)
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
                                                                (into [node (assoc opts :append-softspace? true)] rst)
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
                                                (cond
                                                  (and (= node :Name)
                                                       (#{:Directives :SelectionSet} (ffirst head)))
                                                  (into [node (assoc opts :append-whitespace? true)] rst)

                                                  (and (= node :Arguments)
                                                       (some? (ffirst head)))
                                                  (into [node opts] (concat rst [[:Printable {} " "]]))

                                                  :else x))
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
                                                             (if (or (and (= node :Type)
                                                                          (#{:DefaultValue :Directives} (ffirst head)))
                                                                     (and (= node :DefaultValue)
                                                                          (= (ffirst head) :Directives)))
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
                                                                (into [node (assoc opts :append-softspace? true)]
                                                                      rst)
                                                                x))
                                                    (update :head rest)))
                                                {:acc [:VariableDefinitions opts]
                                                 :head (rest xs)}
                                                xs)))}]
    (insta/transform m ast)))

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

;; public

(defn amend-options
  [ast]
  (->> ast
    (amend-newline-opts)
    (amend-indentation-level-opts)
    (amend-horizontal-spacing-opts)
    (amend-structured-tree-opts)
    (amend-newline-to-structure-tree-opts)
    (amend-prefer-inlining-opts)))

(defn amend-characters-opts
  [ast]
  (letfn [(characters [[node _opts & rst]]
            (cond
              (nil? (first rst)) 0
              (= node :Softline) 0
              (vector? (first rst)) (reduce + (map characters rst))
              (string? (first rst)) (or (and (not= (first rst) "\n")
                                             (count (first rst)))
                                        0)))]
    (let [[node opts & rst] ast]
      (if (= node :Softline)
        (into [node opts] rst)
        (into [node (assoc opts :character-count (characters ast))]
              (cond
                (vector? (first rst)) (map amend-characters-opts rst)
                (string? (first rst)) rst))))))
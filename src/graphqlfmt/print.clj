(ns graphqlfmt.print)

(def +character-limit+ 80)

(defn pr-str-ast
  ([ast]
   (pr-str-ast "" nil ast))
  ([s character-count ast]
   (let [[node opts & rst] ast]
     (let [character-count-exceeded? (and character-count
                                          (> character-count +character-limit+))]
       (apply str (cond
                    (and (= node :Softline) character-count-exceeded?) (map (partial pr-str-ast s character-count) rst)
                    (and (not= node :Softline)
                         (vector? (first rst))) (map (partial pr-str-ast s
                                                              (if (= node :Row)
                                                                (:character-count opts)
                                                                character-count))
                                                     rst)
                    (or (and (= node :Comma) (not character-count-exceeded?))
                        (and (= node :Softspace) (not character-count-exceeded?))
                        (and (not (#{:Comma :Softspace} node))
                             (string? (first rst)))) (str s (first rst))))))))

(defn pr-s
  [ast]
  (->> ast
    (pr-str-ast)
    (clojure.core/format "%s\n")))
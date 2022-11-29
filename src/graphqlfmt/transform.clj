(ns graphqlfmt.transform
  (:require
    [instaparse.core :as insta]
    [graphqlfmt.util :as util]))

(def empty-row [:Row {}])

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
                       (conj acc (util/remove-n-chars line common-indent))))
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

(defn block-string-characters
  [s opts]
  (letfn [(empty-line? [line] (re-find #"^[\u0009\u0020]*$" line))]
    (let [lines (clojure.string/split (block-string-value s) #"\n")]
      (reduce
        (fn [formatted line]
          (if (empty-line? line)
            (str formatted "\n" line)
            (str formatted "\n" (util/indent-s opts) line)))
        (or (if (empty-line? (first lines))
              (str (first lines))
              (str (util/indent-s opts) (first lines)))
            "")
        (rest lines)))))

(defn format-block-string-values
  [ast]
  (let [m {:BlockStringCharacters (fn [opts [_ printable-opts s] & xs]
                                    (into [:BlockStringCharacters opts
                                           [:Printable {} (block-string-characters s printable-opts)]]
                                          xs))}]
    (insta/transform m ast)))

(defn amend-newline-spacing
  [ast]
  (let [[node opts & rst] ast]
    (into (if (not (:prefer-inlining? opts))
            (cond-> [node opts]
              (:prepend-newline? opts) (conj [:Printable {} "\n"])
              (:prepend-indent? opts) (conj [:Printable {} (util/indent-s opts)]))
            [node opts])
          (cond
            (vector? (first rst)) (map amend-newline-spacing
                                       (cond-> rst
                                         (and (not (:prefer-inlining? opts))
                                              (:append-newline? opts)) (concat [[:Printable {} "\n"]])))
            (string? (first rst)) (if (:newline? opts)
                                    [[:Printable {} (first rst)]]
                                    rst)))))

(defn amend-horizontal-spacing
  [ast]
  (let [[node opts & rst] ast]
    (into [node opts]
          (cond
            (vector? (first rst)) (map amend-horizontal-spacing
                                       (cond
                                         (:append-whitespace? opts) (concat rst [[:Printable {} " "]])
                                         (:append-softspace? opts) (concat rst [[:Softspace {} " "]])
                                         :else rst))
            (string? (first rst)) rst))))

(defn amend-softline
  [ast]
  (letfn [(softline [opts]
            [:Softline {}
             [:Newline {} "\n"]
             [:Printable {} (util/indent-s opts)]])]
    (let [m {:Argument (fn [opts & xs]
                         (into [:Argument opts]
                               (conj xs (softline opts))))
             :ParensClose (fn [opts & xs]
                            (into [:ParensClose opts]
                                  (conj xs (softline opts))))
             :VariableDefinition (fn [opts & xs]
                                   (into [:VariableDefinition opts]
                                         (conj xs (softline opts))))}]
      (insta/transform m ast))))

(defn -row-ast!
  [rows row [node opts & rst]]
  (cond
    (nil? rst) [node opts]
    (= node :Softline) (let [softline (into [node opts] rst)]
                         (swap! row conj softline)
                         softline)
    (string? (first rst)) (let [s (first rst)
                                newline? (= s (System/lineSeparator))]
                            (swap! row conj [(cond
                                               newline? :Newline
                                               (#{:Comma :Softspace} node) node
                                               :else :Printable) {} s])
                            (when newline?
                              (swap! rows conj @row)
                              (reset! row empty-row))
                            [node opts s])
    (seq rst) (into [node opts] (mapv (partial -row-ast! rows row) rst))))

;; public

(defn re-transform
  [ast]
  (-> ast
    (format-block-string-values)
    (amend-newline-spacing)
    (amend-horizontal-spacing)
    (amend-softline)))

(defn row-ast
  [ast]
  (let [rows (atom [:Rows {}])
        row (atom empty-row)]
    (-row-ast! rows row ast)
    (when (not= @row empty-row)
      (swap! rows conj @row))
    @rows))

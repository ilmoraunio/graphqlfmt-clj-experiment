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

(defn selection-set [x]
  [:SelectionSet {}
   [:Printable {} "{"]
   x
   [:Printable {} "}"]])

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
                  (conj (into [[:Printable {} "("]] xs)
                        [:Printable {} ")"])))
   :BlockQuote (fn [] "\"\"\"")
   :BlockStringCharacter str
   :BooleanValue boolean-value
   :Comment comment
   :CommentChar str
   :Definition (fn [x] [:Definition {} x])
   :Digit str
   :Document (fn [x] [:Document {} x])
   :EscapedCharacter str
   :EscapedUnicode str
   :ExecutableDefinition (fn [x] [:ExecutableDefinition {} x])
   :ExponentIndicator str
   :ExponentPart str
   :Field (fn [& xs]
            (reduce (fn [coll x] (conj coll x))
                    [:Field {}]
                    xs))
   :FloatValue float-value
   :FractionalPart (partial str ".")
   :IntValue int-value
   :IntegerPart str
   :ListValue (fn [& xs]
                (conj (reduce
                        (fn [coll x] (conj coll x))
                        [:ListValue {} [:Printable {} "["]]
                        (conj (interpose [:Printable {} " "] xs)))
                      [:Printable {} "]"]))
   :Name (fn [x] [:Name {} x])
   :NegativeSign str
   :NonZeroDigit str
   :NullValue null-value
   :ObjectField (fn [x & xs]
                  (reduce (fn [coll x] (conj coll x))
                          [:ObjectField {}
                           x
                           [:Printable {} ":"]]
                          xs))
   :ObjectValue (fn [& xs]
                  (conj (reduce
                          (fn [coll x] (conj coll x))
                          [:ListValue {} [:Printable {} "{"]]
                          (conj (interpose [:Printable {} " "] xs)))
                        [:Printable {} "}"]))
   :OperationDefinition (fn [x] [:OperationDefinition {} x])
   :Quote (fn [] "\"")
   :Selection selection
   :SelectionSet selection-set
   :Sign str
   :StringCharacter str
   :StringValue string-value
   :Value (fn [& xs]
            (reduce (fn [coll x] (conj coll x))
                    [:Value {}]
                    xs))
   :Variable (fn [x] [:Variable {} [:Printable {} "$"] x])})

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
  (clojure.pprint/pprint
    (if (first args)
      (document-parser (first args))
      (document-parser
        (str/join "\n" (line-seq (java.io.BufferedReader. *in*)))))))

(ns graphqlfmt.util)

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
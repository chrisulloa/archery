(ns rarbol.util)

(defn fast-contains?
  "Does an item contain a collection? Short circuits on first match."
  [coll item]
  (if (empty? coll)
    false
    (reduce #(or %1 %2) (map #(= %1 item) coll))))

(defn abs "Absolute value of a number." [n] (max n (- n)))
(ns rarbol.util)

(defn fast-contains?
  [coll item]
  (if (empty? coll)
    false
    (reduce #(or %1 %2) (map #(= %1 item) coll))))

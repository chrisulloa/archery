(ns rarbol.util
  (:require [rarbol.shape :refer [area minimum-bounding-rectangle]]))

(defn fast-contains?
  [coll item]
  (if (empty? coll)
    false
    (reduce #(or %1 %2) (map #(= %1 item) coll))))



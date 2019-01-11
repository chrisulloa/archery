(ns rarbol.util)

(defn fast-contains?
  "Does an item contain a collection? Short circuits on first match."
  [coll item]
  (if (empty? coll)
    false
    (reduce #(or %1 %2) (map #(= %1 item) coll))))

(defn abs "Absolute value of a number." [n] (max n (- n)))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll, removing any elements that
  return duplicate values when passed to a function f."
  ([f]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result x]
          (let [fx (f x)]
            (if (contains? @seen fx)
              result
              (do (vswap! seen conj fx)
                  (rf result x)))))))))
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (let [fx (f x)]
                         (if (contains? seen fx)
                           (recur (rest s) seen)
                           (cons x (step (rest s) (conj seen fx)))))))
                    xs seen)))]
     (step coll #{}))))

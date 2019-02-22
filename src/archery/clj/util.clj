(ns archery.clj.util)

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

(defn fast-min-key
  [f short-circuit-val coll]
  (loop [[val & rest-vals] coll
         min-val {:val val, :f-val ##Inf}]
    (if (nil? val)
      (:val min-val)
      (let [f-val (f val)]
        (if (= f-val short-circuit-val)
          val
          (if (< f-val (:f-val min-val))
            (recur rest-vals {:val val :f-val f-val})
            (recur rest-vals min-val)))))))

(defn fast-max-key
  [f short-circuit-val coll]
  (loop [[val & rest-vals] coll
         max-val {:val val, :f-val ##-Inf}]
    (if (nil? val)
      (:val max-val)
      (let [f-val (f val)]
        (if (= f-val short-circuit-val)
          val
          (if (> f-val (:f-val max-val))
            (recur rest-vals {:val val :f-val f-val})
            (recur rest-vals max-val)))))))

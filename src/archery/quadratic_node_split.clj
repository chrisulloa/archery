(ns archery.quadratic-node-split
  (:require [archery.shape :refer [rectangle-node rectangle-shape
                                   area-enlargement add-child
                                   minimum-bounding-rectangle
                                   leaf? area]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.set :refer [difference]]))
(defprotocol SeedPair
  (inefficiency [_])
  (pair->node [_ leaf?]))

(defrecord QuadraticSeedPair [s1 s2]
  SeedPair
  (inefficiency [_]
    (- (area (minimum-bounding-rectangle s1 s2))
       (area s1) (area s2)))
  (pair->node [_ leaf?]
    (rectangle-node
      leaf? [s1 s2] (rectangle-shape
                      (minimum-bounding-rectangle s1 s2)))))

(defn quadratic-seeds
  [shapes leaf?]
  (letfn [(seed-pair [[s1 s2]] (->QuadraticSeedPair s1 s2))]
    (reduce #(max-key inefficiency %)
            (map seed-pair (combinations shapes 2)))))

(defn shape->seeds
  [shape r-seed l-seed]
  (if (<= (area-enlargement r-seed shape)
          (area-enlargement l-seed shape))
    [(add-child r-seed shape) l-seed]
    [r-seed (add-child l-seed shape)]))

(defn quadratic-split
  [shapes]
  )
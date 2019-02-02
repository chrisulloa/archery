(ns archery.quadratic-node-split
  (:require [archery.shape :refer [rectangle-node rectangle-shape
                                   area-enlargement add-child
                                   minimum-bounding-rectangle
                                   leaf? area]]
            [archery.linear-node-split :refer [split]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.set :refer [difference]]))

(defn inefficiency
  ^double
  [[s1 s2]]
  (- (area (minimum-bounding-rectangle s1 s2))
     (area s1) (area s2)))

(defn pair->seeds
  [leaf? [s1 s2]]
  (letfn [(shape->node [s]
            (rectangle-node leaf? [s] (rectangle-shape s)))]
    [(shape->node s1) (shape->node s2)]))

(defn quadratic-seeds
  [shapes leaf?]
  (pair->seeds leaf? (apply (partial max-key inefficiency)
                            (combinations shapes 2))))

(defn quadratic-split
  [rn min-children]
  (let [children (:children rn)]
    (split (quadratic-seeds children (leaf? rn)) children min-children)))

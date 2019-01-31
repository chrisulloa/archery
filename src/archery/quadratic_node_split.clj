(ns archery.quadratic-node-split
  (:require [archery.shape :refer [rectangle-node rectangle-shape
                                   area-enlargement add-child
                                   minimum-bounding-rectangle
                                   leaf? area]]
            [archery.linear-node-split :refer [split]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.set :refer [difference]]))

(defprotocol QuadraticNodeSplit
  (inefficiency [_])
  (pair->seeds [_ leaf?]))

(defrecord QuadraticSeedPair [s1 s2]
  QuadraticNodeSplit
  (inefficiency ^Double [_]
    (- (area (minimum-bounding-rectangle s1 s2))
       (area s1) (area s2)))
  (pair->seeds [_ leaf?]
    (letfn [(shape->node [s]
              (rectangle-node leaf? [s] (rectangle-shape s)))]
      [(shape->node s1) (shape->node s2)])))

(defn quadratic-seeds
  [shapes leaf?]
  (let [seed-pair (fn [[s1 s2]] (->QuadraticSeedPair s1 s2))]
    (pair->seeds
      (->> (combinations shapes 2)
           (mapv seed-pair)
           (apply (partial max-key inefficiency)))
      leaf?)))

(defn quadratic-split
  [rn min-children]
  (let [children (:children rn)]
    (split (quadratic-seeds children (leaf? rn)) children min-children)))

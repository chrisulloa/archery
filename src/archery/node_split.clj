(ns archery.node-split
  (:require [archery.util :refer [distinct-by]]
            [archery.shape :refer [->Rectangle rectangle-shape leaf?
                                   compress add-child area-enlargement
                                   shape rectangle-node area
                                   minimum-bounding-rectangle]]
             [clojure.math.combinatorics :refer [combinations]]))

(def ^:const inf ##Inf)
(def ^:const -inf ##-Inf)

;; Linear Node Split
(defprotocol LinearNodeSplit
  (update-ns [_ geom] "Updates the ns-map with best seed values.")
  (normalized-separation-x [_] "With the given data computes normalized separation.")
  (normalized-separation-y [_] "With the given data computes normalized separation.")
  (seeds [_ shapes] "Returns seeds, either min-ub/max-lb shapes or two distinct from initial set."))

(defrecord LinearNodeSplitMap [x-max-lb-shape x-min-ub-shape
                               ^double x-max-lb ^double x-min-ub
                               ^double x-min-lb ^double x-max-ub
                               y-max-lb-shape y-min-ub-shape
                               ^double y-max-lb ^double y-min-ub
                               ^double y-min-lb ^double y-max-ub]
  LinearNodeSplit
  (update-ns [ns-map geom]
    (let [rectangle (apply ->Rectangle (rectangle-shape geom))]
      (cond-> ns-map
              (< x-max-lb (:x1 rectangle))
              (assoc :x-max-lb (:x1 rectangle) :x-max-lb-shape geom)
              (> x-min-ub (:x2 rectangle))
              (assoc :x-min-ub (:x2 rectangle) :x-min-ub-shape geom)
              (> x-min-lb (:x1 rectangle))
              (assoc :x-min-lb (:x1 rectangle))
              (< x-max-ub (:x2 rectangle))
              (assoc :x-max-ub (:x2 rectangle))
              (< y-max-lb (:y1 rectangle))
              (assoc :y-max-lb (:y1 rectangle) :y-max-lb-shape geom)
              (> y-min-ub (:y2 rectangle))
              (assoc :y-min-ub (:y2 rectangle) :y-min-ub-shape geom)
              (> y-min-lb (:y1 rectangle))
              (assoc :y-min-lb (:y1 rectangle))
              (< y-max-ub (:y2 rectangle))
              (assoc :y-max-ub (:y2 rectangle)))))
  (normalized-separation-x ^double [_]
    (if (= x-max-lb-shape x-min-ub-shape)
      inf (/ (- x-max-lb x-min-ub) (- x-max-ub x-min-lb))))
  (normalized-separation-y ^double [_]
    (if (= y-max-lb-shape y-min-ub-shape)
      inf (/ (- y-max-lb y-min-ub) (- y-max-ub y-min-lb))))
  (seeds [ns-map shapes]
    (let [norm-sep-x (normalized-separation-x ns-map)
          norm-sep-y (normalized-separation-y ns-map)]
      (if (> norm-sep-x norm-sep-y)
        (if (Double/isInfinite norm-sep-x)
          (take 2 (distinct-by shape shapes))
          [y-max-lb-shape y-min-ub-shape])
        (if (Double/isInfinite norm-sep-y)
          (take 2 (distinct-by shape shapes))
          [x-max-lb-shape x-min-ub-shape])))))

(def initial-node-split-map
  (map->LinearNodeSplitMap
    {:x-min-ub-shape nil, :x-max-lb-shape nil
     :x-max-lb -inf, :x-min-ub inf,
     :x-min-lb inf, :x-max-ub -inf,
     :y-min-ub-shape nil, :y-max-lb-shape nil
     :y-max-lb -inf, :y-min-ub inf
     :y-min-lb inf, :y-max-ub -inf}))

(defn linear-seeds
  [shapes leaf?]
  (letfn [(shape->node [s]
            (rectangle-node leaf? [s] (rectangle-shape s)))]
    (loop [[geom & geoms] shapes
           ns-map initial-node-split-map]
      (if-not (nil? geom)
        (recur geoms (update-ns ns-map geom))
        (mapv shape->node (seeds ns-map shapes))))))

;; Quadratic Node Split
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

;; Node Splitting functions that take in either quadratic or linear seeds.

(defn shape->seeds
  [shape r-seed l-seed]
  (if (<= (area-enlargement r-seed shape)
          (area-enlargement l-seed shape))
    [(add-child r-seed shape) l-seed]
    [r-seed (add-child l-seed shape)]))

(defn split
  [seeds children min-children]
  (loop [r-seed (seeds 0)
         l-seed (seeds 1)
         shapes (into [] (remove #{(first (:children r-seed))
                                   (first (:children l-seed))}) children)]
    (if-not (empty? shapes)
      (cond
        (= min-children (+ (count (:children r-seed)) (count shapes)))
        (recur (reduce add-child r-seed shapes)
               l-seed
               nil)
        (= min-children (+ (count (:children l-seed)) (count shapes)))
        (recur r-seed
               (reduce add-child l-seed shapes)
               nil)
        :else
        (let [next-seeds (shape->seeds (first shapes) r-seed l-seed)]
          (recur (next-seeds 0) (next-seeds 1) (rest shapes))))
      [(compress r-seed) (compress l-seed)])))

(defn linear-split
  [rn min-children]
  (let [children (:children rn)]
    (split (linear-seeds children (leaf? rn)) children min-children)))

(defn quadratic-split
  [rn min-children]
  (let [children (:children rn)]
    (split (quadratic-seeds children (leaf? rn)) children min-children)))

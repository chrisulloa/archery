(ns archery.shape
  (:require [archery.util :refer [abs fast-contains? distinct-by fast-min-by fast-max-by]]
            [clojure.core.protocols :refer [Datafiable datafy]])
  (:import (clojure.lang IPersistentCollection)))

(defprotocol TreeNode
  (leaf? [node] "Is this node a leaf?")
  (branch? [node] "Can this node have children?")
  (count-children [node] "Count children.")
  (children [node] "Children of node.")
  (children-nodes [node] "Children nodes of the node.")
  (make-node [node children] "Makes new node from existing node and new children."))

(defprotocol Geometry
  (dim [geom] "Dimension of the given geometry")
  (area [geom] "Area of the given geometry")
  (shape [geom] "The defined shape of the geometry.")
  (collect-points [geom] "Points of a given geometry."))

(defrecord Rectangle [shape]
  Datafiable
  (datafy [_] {:type :Rectangle, :shape shape})
  Geometry
  (dim [_] (count shape))
  (area [_] (transduce (map #(reduce - (reverse %))) * shape))
  (shape [_] shape)
  (collect-points [_] shape)
  TreeNode
  (branch? [_] false)
  (children [_] nil))

(defrecord Point [shape]
  Datafiable
  (datafy [_] {:type :Point, :shape shape})
  Geometry
  (dim [_] (count shape))
  (area [_] 0)
  (shape [_] shape)
  (collect-points [_] (map (partial repeat 2) shape))
  TreeNode
  (branch? [_] false)
  (children [_] nil))

(deftype RectangleNode [^boolean leaf? ^IPersistentCollection children ^IPersistentCollection shape]
  Datafiable
  (datafy [_] {:type :RectangleNode,
               :leaf? leaf?,
               :shape shape,
               :children (mapv datafy children)})
  Geometry
  (dim [_] (count shape))
  (area [_] (transduce (map #(reduce - (reverse %))) * shape))
  (shape [_] shape)
  (collect-points [_] shape)
  TreeNode
  (leaf? [_] leaf?)
  (branch? [_] true)
  (count-children [_] (count children))
  (children [_] children)
  (children-nodes [_] (when-not leaf? children))
  (make-node [_ new-children] (RectangleNode. leaf? new-children shape)))

(defrecord RTree [^RectangleNode root dimension max-children min-children]
  Datafiable
  (datafy [_] {:type :RTree
               :root (datafy root),
               :dimension dimension,
               :max-children max-children,
               :min-children min-children}))

(defn rtree
  ([]
   (map->RTree
     {:root (->RectangleNode true [] []), :max-children 4, :min-children 2, :dimension 2}))
  ([params]
   (merge (rtree) params)))

(defmulti envelops? (fn [x y] [(class x) (class y)]))

(defmethod envelops? [Point Rectangle] [_ _] false)

(defmethod envelops? [Point RectangleNode] [_ _] false)

(defmethod envelops? [Rectangle Point]
  [r p]
  (letfn [(envelops-by-dim? [[[r-min r-max] p]]
            (<= r-min p r-max))]
    (->> (interleave (:shape r) (:shape p))
         (partition 2)
         (map envelops-by-dim?)
         (every? true?))))

(defmethod envelops? [RectangleNode Point]
  [rn p]
  (envelops? (->Rectangle (shape rn)) p))

(defmethod envelops? [Rectangle Rectangle]
  [r1 r2]
  (letfn [(envelops-by-dim? [[[r1-min r1-max] [r2-min r2-max]]]
            (<= r1-min r2-min r2-max r1-max))]
    (->> (interleave (:shape r1) (:shape r2))
         (partition 2)
         (map envelops-by-dim?)
         (every? true?))))

(defmethod envelops? [RectangleNode RectangleNode]
  [rn1 rn2]
  (envelops? (->Rectangle (shape rn1)) (->Rectangle (shape rn2))))

(defmethod envelops? [RectangleNode Rectangle]
  [rn r]
  (envelops? (->Rectangle (shape rn)) r))

(defmethod envelops? [Rectangle RectangleNode]
  [r rn]
  (envelops? r (->Rectangle (shape rn))))

(defmethod envelops? [Point Point]
  [p1 p2] (= (:shape p1) (:shape p2)))

(defmulti intersects?
          (fn [x y] [(class x) (class y)]))

(defmethod intersects? [Rectangle Rectangle]
  [r1 r2]
  (letfn [(intersects-by-dim?
            [[r1p r2p]]
            (not (or (> (first r1p) (second r2p))
                     (> (first r2p) (second r1p)))))]
    (->> (interleave (:shape r1) (:shape r2))
         (partition 2)
         (map intersects-by-dim?)
         (some true?)
         (nil?)
         (not))))

(defmethod intersects? [RectangleNode RectangleNode]
  [rn1 rn2]
  (intersects? (->Rectangle (shape rn1)) (->Rectangle (shape rn2))))

(defmethod intersects? [RectangleNode Rectangle]
  [rn r]
  (intersects? (->Rectangle (shape rn)) r))

(defmethod intersects? [Rectangle RectangleNode]
  [r rn]
  (intersects? (->Rectangle (shape rn)) r))

(defmethod intersects? [Point Point]
  [p1 p2] (= (:shape p1) (:shape p2)))

(defmethod intersects? [Rectangle Point]
  [r p]
  (envelops? r p))

(defmethod intersects? [RectangleNode Point]
  [rn p]
  (envelops? (->Rectangle (shape rn)) p))

(defmethod intersects? [Point Rectangle]
  [p r] (intersects? r p))

(defmethod intersects? [Point RectangleNode]
  [p rn] (intersects? (shape rn) p))

(defn minimum-bounding-rectangle
  ([s] (->Rectangle (collect-points s)))
  ([s1 s2]
   (->Rectangle
     (into [] (map (juxt #(reduce min %) #(reduce max %)))
           (map concat (collect-points s1) (collect-points s2)))))
  ([s1 s2 & shapes]
    (reduce minimum-bounding-rectangle (minimum-bounding-rectangle s1 s2) shapes)))

(defn compress-node
  "Adjusts boundary for tight fit, after adding extra shapes if needed."
  ([rn]
   (let [children (children rn)]
     (if-not (empty? children)
       (RectangleNode. (leaf? rn) children (shape (apply minimum-bounding-rectangle children)))
       rn)))
  ([rn geom]
   (let [children (conj (children rn) geom)]
     (if-not (empty? children)
       (RectangleNode. (leaf? rn) children (shape (apply minimum-bounding-rectangle children)))
       rn)))
  ([rn geom & geoms]
   (reduce compress-node (compress-node rn geom) geoms)))

(defn area-enlargement-diff
  "Difference in area of rectangle node before and after
   enlargement with a shape"
  [^RectangleNode node shape]
  (- (area (minimum-bounding-rectangle node shape))
     (area node)))

(defn best-node-for-insertion
  [nodes shape-to-insert]
  (loop [[node & rest-nodes] nodes
         best-node {:shape [], :area-diff ##Inf}]
    (if node
      (let [area-diff (area-enlargement-diff node shape-to-insert)]
        (if (zero? area-diff)
          (shape node)
          (if (< area-diff (:area-diff best-node))
            (recur rest-nodes {:shape (shape node), :area-diff area-diff})
            (recur rest-nodes best-node))))
      (:shape best-node))))

(defn linear-seeds-across-dimensions
  [shapes]
  (for [d (range (dim (first shapes)))]
    (let [max-lb (apply max-key (comp first #(nth % d) collect-points) shapes)
          min-ub (apply min-key (comp second #(nth % d) collect-points) shapes)]
      (if (= (shape max-lb) (shape min-ub))
        (let [distinct-shapes (distinct-by shape shapes)]
          {:norm-separation ##Inf,
           :seeds           [(first distinct-shapes) (second distinct-shapes)]})
        {:norm-separation (/ (- (-> max-lb collect-points (nth d) first)
                                (-> min-ub collect-points (nth d) second))
                             (- (apply max (map (comp second #(nth % d) collect-points) shapes))
                                (apply min (map (comp first #(nth % d) collect-points) shapes))))
         :seeds           [min-ub max-lb]}))))

(defn linear-seeds
  [shapes leaf?]
  (->> shapes
       (linear-seeds-across-dimensions)
       (fast-max-by :norm-separation ##Inf)
       (:seeds)
       (map #(RectangleNode. leaf? [%] (shape %)))))

(defn shape->seeds
  [shape r-seed l-seed]
  (if (<= (area-enlargement-diff r-seed shape)
          (area-enlargement-diff l-seed shape))
    [(compress-node r-seed shape) l-seed]
    [r-seed (compress-node l-seed shape)]))

(defn linear-split [rn min-children]
  (let [seeds (linear-seeds (children rn) (leaf? rn))]
    (loop [r-seed (first seeds)
           l-seed (second seeds)
           shapes (remove #{(-> r-seed children first)
                            (-> l-seed children first)} (children rn))]
      (if-not (empty? shapes)
        (cond
          (= min-children (+ (count-children r-seed) (count shapes)))
          (recur (apply compress-node r-seed shapes)
                 l-seed
                 nil)
          (= min-children (+ (count-children l-seed) (count shapes)))
          (recur r-seed
                 (apply compress-node l-seed shapes)
                 nil)
          :else
          (let [next-seeds (shape->seeds (first shapes) r-seed l-seed)]
            (recur (first next-seeds) (second next-seeds) (rest shapes))))
        [r-seed l-seed]))))
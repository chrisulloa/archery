(ns archery.shape
  (:require [archery.util :refer [abs fast-contains? distinct-by fast-min-by]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.protocols :refer [Datafiable datafy]]))

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
  (area [_]
    (apply * (map #(- (second %) (first %)) shape)))
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
  (collect-points [_] (map vector shape))
  TreeNode
  (branch? [_] false)
  (children [_] nil))

(deftype RectangleNode [leaf? children shape]
  Datafiable
  (datafy [_] {:type :RectangleNode,
               :leaf? leaf?,
               :shape shape,
               :children (mapv datafy children)})
  Geometry
  (dim [_] (count shape))
  (area [_] (apply * (map #(- (second %) (first %)) shape)))
  (shape [_] shape)
  (collect-points [_] shape)
  TreeNode
  (leaf? [_] leaf?)
  (branch? [_] true)
  (count-children [_] (count children))
  (children [_] children)
  (children-nodes [_] (when-not leaf? children))
  (make-node [_ new-children] (RectangleNode. leaf? new-children shape)))

(defrecord RTree [root dimension max-children min-children]
  Datafiable
  (datafy [_] {:root (datafy root)
               :dimension dimension
               :max-children max-children
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
  "Given a shape or collection of shapes, computes the minimum
   bounding rectangle. collect-points must be defined for a shape."
  [& shapes]
  (->> shapes
       (map collect-points)
       (reduce #(map conj %1 %2))
       (map flatten)
       (map #(vector (reduce min %) (reduce max %)))))

(defn shape->rectangle
  "Coerces a shape to a rectangle given its minimum boundary."
  [shape]
  (if (or (instance? RectangleNode shape) (instance? Rectangle shape))
    shape
    (->Rectangle (minimum-bounding-rectangle shape))))

(defn area-enlargement-diff
  "Difference in area of rectangle node before and after
   enlargement with a shape"
  [node shape]
  (- (area (->Rectangle (minimum-bounding-rectangle shape node)))
     (area node)))

(defn best-node-for-insertion
  [nodes shape-to-insert]
  (some->> nodes
           (map #(hash-map :node-shape (shape %)
                           :diff (area-enlargement-diff % shape-to-insert)))
           (fast-min-by :diff 0)
           (:node-shape)))

(defn compress-node
  "Adjusts boundary for tight fit, after adding extra shapes if needed."
  ([rn]
   (let [children (children rn)]
     (if-not (empty? children)
       (RectangleNode. (leaf? rn) children (apply minimum-bounding-rectangle children))
       rn)))
  ([rn shape]
   (let [children (conj (children rn) shape)]
     (if-not (empty? children)
       (RectangleNode. (leaf? rn) children (apply minimum-bounding-rectangle children))
       rn)))
  ([rn shape & shapes]
   (reduce compress-node (compress-node rn shape) shapes)))

(defn augment-shape
  "Augments a shape by creating a map of sides along a dimension.
    e.g. (->Rectangle [[0 10] [5 15]]) => {0 [0 5] 1, [10 15]}
         (->Point [10 30]) => {0 [10 30], 1 [30 30}
   For use in calculating highest-low-side, lowest-high-side, etc."
  [shape]
  {:shape shape
   :augmented (zipmap (range (dim shape))
                      (collect-points (shape->rectangle shape)))})

(defn augmented-val
  "Retrieves the (first or second) position along a
   dimension of an augmented shape."
  [position dimension]
  (fn [shape] (-> shape :augmented (get dimension) position)))

(defn linear-seeds-across-dimensions
  [shapes]
  (let [dimensions (dim (first shapes))
        reduced-shapes (map augment-shape shapes)
        min-or-max-side #(apply (partial %1 (augmented-val %2 %3)) reduced-shapes)]
    (for [d (range dimensions)]
      (let [max-lb (min-or-max-side max-key first d)
            min-lb (min-or-max-side min-key first d)
            max-ub (min-or-max-side max-key second d)
            min-ub (min-or-max-side min-key second d)]
        (if (= (shape (:shape max-lb)) (shape (:shape min-ub)))
          (let [reduced-distinct-shapes (distinct-by #(shape (:shape %)) reduced-shapes)]
            {:dimension       d,
             :norm-separation ##Inf,
             :seeds           [(compress-node (:shape (first reduced-distinct-shapes)))
                               (compress-node (:shape (second reduced-distinct-shapes)))]})
          {:dimension       d
           :norm-separation (/ (- ((augmented-val first d) max-lb)
                                  ((augmented-val second d) min-ub))
                               (- ((augmented-val second d) max-ub)
                                  ((augmented-val first d) min-lb)))
           :seeds           [(compress-node (:shape min-ub))
                             (compress-node (:shape max-lb))]})))))

(defn initialize-seed
  "Creates a bounding box around a seed shape and includes it in vals."
  [seed leaf?]
  (->> seed
       minimum-bounding-rectangle
       (RectangleNode. leaf? [seed])))

(defn linear-seeds
  [shapes leaf?]
  (->> shapes
       linear-seeds-across-dimensions
       (apply (partial max-key :norm-separation))
       (:seeds)
       (map #(initialize-seed % leaf?))))

(defn shape->seeds
  [shape r-seed l-seed]
  (if (<= (area-enlargement-diff r-seed shape)
          (area-enlargement-diff l-seed shape))
    [(compress-node r-seed shape) l-seed]
    [r-seed (compress-node l-seed shape)]))

(defn linear-split [rn min-children]
  (when-let [shapes (children rn)]
    (let [seeds (linear-seeds shapes (leaf? rn))]
      (loop [r-seed (first seeds)
             l-seed (second seeds)
             [shape & rest-shapes] (remove #{(-> r-seed children first)
                                             (-> l-seed children first)} shapes)]
        (if shape
          (let [next-seeds (shape->seeds shape r-seed l-seed)]
            (recur (first next-seeds) (second next-seeds) rest-shapes))
          (compress-node (->RectangleNode false [] []) r-seed l-seed))))))
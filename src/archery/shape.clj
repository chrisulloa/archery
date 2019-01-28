(ns archery.shape
  (:require [archery.util :refer [distinct-by fast-min-key]]
            [clojure.core.protocols :refer [Datafiable datafy]]))

(defprotocol TreeNode
  (leaf? [node] "Is this node a leaf?")
  (branch? [node] "Can this node have children?")
  (compress [node] "Compress this node's shape to fit its children.")
  (reshape [node new-shape] "Clone this node with new shape.")
  (add-child [node child] "Add a child to the node.")
  (choose-child-for-insert [node shape] "Find best child node to insert shape into.")
  (children-nodes [node] "Children nodes of the node.")
  (make-node [node children] "Makes new node from existing node and new children."))

(defprotocol Geometry
  (area-enlargement [geom1 geom2] "Area of mbr around this shape and the other")
  (area [geom] "Area of the given geometry")
  (shape [geom] "The defined shape of the geometry.")
  (rectangle-shape [geom] "Points of a given geometry."))

(defrecord Rectangle [x1 y1 x2 y2]
  Datafiable
  (datafy [_] {:type :Rectangle, :shape [x1 y1 x2 y2]})
  Geometry
  (area [_] (* (- x2 x1) (- y2 y1)))
  (shape [_] [x1 y1 x2 y2])
  (rectangle-shape [_] [x1 y1 x2 y2]))

(defrecord Point [x y]
  Datafiable
  (datafy [_] {:type :Point, :shape [x y]})
  Geometry
  (area [_] 0)
  (shape [_] [x y])
  (rectangle-shape [_] [x y x y]))

(defn minimum-bounding-rectangle
  ([s] (apply ->Rectangle (rectangle-shape s)))
  ([s1 s2]
   (let [[s1-x1 s1-y1 s1-x2 s1-y2] (rectangle-shape s1)
         [s2-x1 s2-y1 s2-x2 s2-y2] (rectangle-shape s2)]
     (->Rectangle (min s1-x1 s2-x1) (min s1-y1 s2-y1)
                  (max s1-x2 s2-x2) (max s1-y2 s2-y2))))
  ([s1 s2 & shapes]
   (reduce minimum-bounding-rectangle
           (minimum-bounding-rectangle s1 s2)
           shapes)))

(defrecord RectangleNode [leaf? children x1 y1 x2 y2]
  Datafiable
  (datafy [_] {:type :RectangleNode,
               :leaf? leaf?,
               :shape [x1 y1 x2 y2],
               :children (mapv datafy children)})
  Geometry
  (area [_] (* (- x2 x1) (- y2 y1)))
  (area-enlargement [node geom]
    (let [[s-x1 s-y1 s-x2 s-y2] (rectangle-shape geom)]
      (- (* (- (max x2 s-x2) (min x1 s-x1))
            (- (max y2 s-y2) (min y1 s-y1)))
         (* (area node)))))
  (shape [_] [x1 y1 x2 y2])
  (rectangle-shape [_] [x1 y1 x2 y2])
  TreeNode
  (reshape [_ [dx1 dy1 dx2 dy2]]
    (->RectangleNode leaf? children dx1 dy1 dx2 dy2))
  (choose-child-for-insert [_ geom]
    (when-not leaf?
      (fast-min-key #(area-enlargement % geom) 0 children)))
  (compress [node]
    (if (empty? children)
      node
      (->> children
           (apply minimum-bounding-rectangle)
           (shape)
           (reshape node))))
  (leaf? [_] leaf?)
  (branch? [_] true)
  (children-nodes [_] (when-not leaf? children))
  (add-child [_ child]
    (->RectangleNode leaf? (conj children child) x1 y1 x2 y2))
  (make-node [_ new-children]
    (->RectangleNode leaf? new-children x1 y1 x2 y2)))

(defn rectangle-node
  [leaf? children [x1 y1 x2 y2]]
  (->RectangleNode leaf? children x1 y1 x2 y2))

(defrecord RTree [root max-children min-children]
  Datafiable
  (datafy [_] {:type :RTree,
               :max-children max-children,
               :min-children min-children
               :root (datafy root)}))

(defmulti envelops? (fn [x y] [(class x) (class y)]))

(defmethod envelops? [Point Rectangle] [_ _] false)

(defmethod envelops? [Point RectangleNode] [_ _] false)

(defmethod envelops? [Rectangle Point]
  [r p]
  (and (<= (:x1 r) (:x p) (:x2 r)) (<= (:y1 r) (:y p) (:y2 r))))

(defmethod envelops? [RectangleNode Point]
  [r p]
  (and (<= (:x1 r) (:x p) (:x2 r)) (<= (:y1 r) (:y p) (:y2 r))))

(defmethod envelops? [Rectangle Rectangle]
  [r1 r2]
  (and (<= (:x1 r1) (:x1 r2) (:x2 r2) (:x2 r1))
       (<= (:y1 r1) (:y1 r2) (:y2 r2) (:y2 r1))))

(defmethod envelops? [RectangleNode RectangleNode]
  [r1 r2]
  (and (<= (:x1 r1) (:x1 r2) (:x2 r2) (:x2 r1))
       (<= (:y1 r1) (:y1 r2) (:y2 r2) (:y2 r1))))

(defmethod envelops? [RectangleNode Rectangle]
  [r1 r2]
  (and (<= (:x1 r1) (:x1 r2) (:x2 r2) (:x2 r1))
       (<= (:y1 r1) (:y1 r2) (:y2 r2) (:y2 r1))))

(defmethod envelops? [Rectangle RectangleNode]
  [r1 r2]
  (and (<= (:x1 r1) (:x1 r2) (:x2 r2) (:x2 r1))
       (<= (:y1 r1) (:y1 r2) (:y2 r2) (:y2 r1))))

(defmethod envelops? [Point Point] [p1 p2] (= (shape p1) (shape p2)))

(defmulti intersects? (fn [x y] [(class x) (class y)]))

(defmethod intersects? [Rectangle Rectangle]
  [r1 r2]
  (and (not (or (> (:x1 r1) (:x2 r2))
                (> (:x1 r2) (:x2 r1))))
       (not (or (> (:y1 r1) (:y2 r2))
                (> (:y1 r2) (:y2 r1))))))

(defmethod intersects? [RectangleNode RectangleNode]
  [r1 r2]
  (and (not (or (> (:x1 r1) (:x2 r2))
                (> (:x1 r2) (:x2 r1))))
       (not (or (> (:y1 r1) (:y2 r2))
                (> (:y1 r2) (:y2 r1))))))

(defmethod intersects? [RectangleNode Rectangle]
  [r1 r2]
  (and (not (or (> (:x1 r1) (:x2 r2))
                (> (:x1 r2) (:x2 r1))))
       (not (or (> (:y1 r1) (:y2 r2))
                (> (:y1 r2) (:y2 r1))))))

(defmethod intersects? [Rectangle RectangleNode]
  [r1 r2]
  (and (not (or (> (:x1 r1) (:x2 r2))
                (> (:x1 r2) (:x2 r1))))
       (not (or (> (:y1 r1) (:y2 r2))
                (> (:y1 r2) (:y2 r1))))))

(defmethod intersects? [Point Point] [p1 p2] (= (:shape p1) (:shape p2)))

(defmethod intersects? [Rectangle Point] [r p] (envelops? r p))

(defmethod intersects? [RectangleNode Point] [rn p] (envelops? rn p))

(defmethod intersects? [Point Rectangle] [p r] (intersects? r p))

(defmethod intersects? [Point RectangleNode] [p rn] (intersects? rn p))

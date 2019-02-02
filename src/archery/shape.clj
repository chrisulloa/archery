(ns archery.shape
  (:require [archery.util :refer [distinct-by fast-min-key]]
            [clojure.core.protocols :refer [Datafiable datafy]]))

(defn double-max ^double
[^double a ^double b]
  (Math/max a b))

(defn double-min ^double
[^double a ^double b]
  (Math/min a b))

(defn double-area ^double
[^double x1 ^double y1 ^double x2 ^double y2]
  (* (- x2 x1) (- y2 y1)))

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
  (minimum-bounding-rectangle [geom] [geom1 geom2] "Minimum bounding rectangle of the geoms.")
  (area-enlargement [geom1 geom2] "Area of minimum-bounding-rectangle around this shape and the other")
  (area [geom] "Area of the given geometry")
  (shape [geom] "The defined shape of the geometry.")
  (rectangle-shape [geom] "Points of a given geometry."))

(extend-protocol Geometry
  nil
  (minimum-bounding-rectangle [_ geom]
    (minimum-bounding-rectangle geom)))

(defrecord Rectangle [^double x1 ^double y1 ^double x2 ^double y2]
  Datafiable
  (datafy [_] {:type :Rectangle, :shape [x1 y1 x2 y2]})
  Geometry
  (minimum-bounding-rectangle [r] r)
  (minimum-bounding-rectangle [_ geom]
    (let [[geom-x1 geom-y1 geom-x2 geom-y2] (rectangle-shape geom)]
      (->Rectangle (double-min x1 geom-x1) (double-min y1 geom-y1)
                   (double-max x2 geom-x2) (double-max y2 geom-y2))))
  (area ^double [_] (double-area x1 y1 x2 y2))
  (shape [_] [x1 y1 x2 y2])
  (rectangle-shape [_] [x1 y1 x2 y2]))

(defrecord Point [^double x ^double y]
  Datafiable
  (datafy [_] {:type :Point, :shape [x y]})
  Geometry
  (minimum-bounding-rectangle [_] (->Rectangle x y x y))
  (minimum-bounding-rectangle [_ geom]
    (let [[geom-x1 geom-y1 geom-x2 geom-y2] (rectangle-shape geom)]
      (->Rectangle (double-min x geom-x1) (double-min y geom-y1)
                   (double-max x geom-x2) (double-max y geom-y2))))
  (area ^double [_] 0.0)
  (shape [_] [x y])
  (rectangle-shape [_] [x y x y]))

(defrecord RectangleNode [leaf? children
                          ^double x1 ^double y1
                          ^double x2 ^double y2]
  Datafiable
  (datafy [_] {:type :RectangleNode,
               :leaf? leaf?,
               :shape [x1 y1 x2 y2],
               :children (mapv datafy children)})
  Geometry
  (minimum-bounding-rectangle [_] (->Rectangle x1 y1 x2 y2))
  (minimum-bounding-rectangle [_ geom]
    (let [[geom-x1 geom-y1 geom-x2 geom-y2] (rectangle-shape geom)]
      (->Rectangle (double-min x1 geom-x1) (double-min y1 geom-y1)
                   (double-max x2 geom-x2) (double-max y2 geom-y2))))
  (area ^double [_] (double-area x1 y1 x2 y2))
  (area-enlargement ^double [node geom]
    (let [[s-x1 s-y1 s-x2 s-y2] (rectangle-shape geom)]
      (- (* (- (double-max x2 s-x2) (double-min x1 s-x1))
            (- (double-max y2 s-y2) (double-min y1 s-y1)))
         (* (area node)))))
  (shape [_] [x1 y1 x2 y2])
  (rectangle-shape [_] [x1 y1 x2 y2])
  TreeNode
  (reshape [_ [dx1 dy1 dx2 dy2]]
    (->RectangleNode leaf? children dx1 dy1 dx2 dy2))
  (choose-child-for-insert [_ geom]
    (fast-min-key #(area-enlargement % geom) 0.0 children))
  (compress [node]
    (if (empty? children)
      node
      (reshape
        node (shape (reduce minimum-bounding-rectangle nil children)))))
  (leaf? [_] leaf?)
  (branch? [_] true)
  (children-nodes [_] (when-not leaf? children))
  (add-child [_ child]
    (->RectangleNode leaf? (conj children child) x1 y1 x2 y2))
  (make-node [_ new-children]
    (->RectangleNode leaf? new-children x1 y1 x2 y2)))

(defn rectangle-node
  [leaf? children shapev]
  (apply ->RectangleNode leaf? children shapev))

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

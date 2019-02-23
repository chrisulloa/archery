(ns archery.clj.shape
  (:require [archery.clj.util :refer [distinct-by fast-min-key]]
            [clojure.core.protocols :refer [Datafiable datafy]])
  (:import (archery.util MutableRectangleNode)))

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
  (children [node] "Children of node")
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

(extend-type MutableRectangleNode
  Datafiable
  (datafy [^MutableRectangleNode node]
    {:type :MutableRectangleNode,
     :leaf? (.isLeaf node),
     :shape (shape node)
     :children (.getChildren node)})
  Geometry
  (minimum-bounding-rectangle
    ([node]
     (->Rectangle (.getXMin node) (.getYMin node)
                  (.getXMax node) (.getYMax node)))
    ([node geom]
     (let [[geom-x1 geom-y1 geom-x2 geom-y2] (rectangle-shape geom)]
       (->Rectangle (double-min (.getXMin node) geom-x1)
                    (double-min (.getYMin node) geom-y1)
                    (double-max (.getXMax node) geom-x2)
                    (double-max (.getYMax node) geom-y2)))))
  (area ^double [node] (double-area (.getXMin node) (.getYMin node)
                                    (.getXMax node) (.getYMax node)))
  (area-enlargement ^double [node geom]
    (let [[s-x1 s-y1 s-x2 s-y2] (rectangle-shape geom)]
      (- (* (- (double-max (.getXMax node) s-x2)
               (double-min (.getXMin node) s-x1))
            (- (double-max (.getYMax node) s-y2)
               (double-min (.getYMin node) s-y1)))
         (* (area node)))))
  (shape [node] [(.getXMin node) (.getYMin node)
                 (.getXMax node) (.getYMax node)])
  (rectangle-shape [node] (shape node))
  TreeNode
  (reshape ^MutableRectangleNode [^MutableRectangleNode node [dx1 dy1 dx2 dy2]]
    (MutableRectangleNode/reshape node dx1 dy1 dx2 dy2))
  (choose-child-for-insert [^MutableRectangleNode node geom]
    (fast-min-key #(area-enlargement % geom) 0.0 (.getChildren node)))
  (compress [^MutableRectangleNode node]
    (let [children (children node)]
      (if (empty? children)
        node
        (reshape
          node (shape (reduce minimum-bounding-rectangle nil children))))))
  (leaf? [node] (.isLeaf node))
  (branch? [node] (.isBranch node))
  (children [node] (.getChildren node))
  (children-nodes [node] (.getChildrenNodes node))
  (add-child [node child]
    (MutableRectangleNode/addChild node child))
  (make-node [node new-children]
    (MutableRectangleNode/setChildren node new-children)))

;(defrecord RectangleNode [leaf? children
;                          ^double x1 ^double y1
;                          ^double x2 ^double y2]
;  Datafiable
;  (datafy [_] {:type :RectangleNode,
;               :leaf? leaf?,
;               :shape [x1 y1 x2 y2],
;               :children (mapv datafy children)})
;  Geometry
;  (minimum-bounding-rectangle [_] (->Rectangle x1 y1 x2 y2))
;  (minimum-bounding-rectangle [_ geom]
;    (let [[geom-x1 geom-y1 geom-x2 geom-y2] (rectangle-shape geom)]
;      (->Rectangle (double-min x1 geom-x1) (double-min y1 geom-y1)
;                   (double-max x2 geom-x2) (double-max y2 geom-y2))))
;  (area ^double [_] (double-area x1 y1 x2 y2))
;  (area-enlargement ^double [node geom]
;    (let [[s-x1 s-y1 s-x2 s-y2] (rectangle-shape geom)]
;      (- (* (- (double-max x2 s-x2) (double-min x1 s-x1))
;            (- (double-max y2 s-y2) (double-min y1 s-y1)))
;         (* (area node)))))
;  (shape [_] [x1 y1 x2 y2])
;  (rectangle-shape [_] [x1 y1 x2 y2])
;  TreeNode
;  (reshape [_ [dx1 dy1 dx2 dy2]]
;    (->RectangleNode leaf? children dx1 dy1 dx2 dy2))
;  (choose-child-for-insert [_ geom]
;    (fast-min-key #(area-enlargement % geom) 0.0 children))
;  (compress [node]
;    (if (empty? children)
;      node
;      (reshape
;        node (shape (reduce minimum-bounding-rectangle nil children)))))
;  (leaf? [_] leaf?)
;  (branch? [_] true)
;  (children-nodes [_] (when-not leaf? children))
;  (add-child [_ child]
;    (->RectangleNode leaf? (conj children child) x1 y1 x2 y2))
;  (make-node [_ new-children]
;    (->RectangleNode leaf? new-children x1 y1 x2 y2)))

(defn mutable-node
  ^MutableRectangleNode
  ([leaf? x1 y1 x2 y2]
   (MutableRectangleNode. leaf? x1 y1 x2 y2))
  ([leaf? children x1 y1 x2 y2]
    (let [node (mutable-node leaf? x1 y1 x2 y2)]
      (reduce add-child node children))))

(defn rectangle-node
  [leaf? children shapev]
  (apply mutable-node leaf? children shapev))

(defmulti envelops? (fn [x y] [(class x) (class y)]))

(defmethod envelops? [Point Rectangle] [_ _] false)

(defmethod envelops? [Point MutableRectangleNode] [_ _] false)

(defmethod envelops? [Rectangle Point]
  [r p]
  (and (<= (:x1 r) (:x p) (:x2 r)) (<= (:y1 r) (:y p) (:y2 r))))

(defmethod envelops? [MutableRectangleNode Point]
  [r p]
  (and (<= (.getXMin r) (:x p) (.getXMax r)) (<= (.getYMin r) (:y p) (.getYMax r))))

(defmethod envelops? [Rectangle Rectangle]
  [r1 r2]
  (and (<= (:x1 r1) (:x1 r2) (:x2 r2) (:x2 r1))
       (<= (:y1 r1) (:y1 r2) (:y2 r2) (:y2 r1))))

(defmethod envelops? [MutableRectangleNode MutableRectangleNode]
  [r1 r2]
  (and (<= (.getXMin r1) (.getXMin r2) (.getXMax r2) (.getXMax r1))
       (<= (.getYMin r1) (.getYMin  r2) (.getYMin r2) (.getYMin r1))))

(defmethod envelops? [MutableRectangleNode Rectangle]
  [r1 r2]
  (and (<= (.getXMin r1) (:x1 r2) (:x2 r2) (.getXMax r1))
       (<= (.getYMin r1) (:y1 r2) (:y2 r2) (.getYMax r1))))

(defmethod envelops? [Rectangle MutableRectangleNode]
  [r1 r2]
  (and (<= (:x1 r1) (.getXMin r2) (.getXMax r2) (:x2 r1))
       (<= (:y1 r1) (.getYMin r2) (.getYMax r2) (:y2 r1))))

(defmethod envelops? [Point Point] [p1 p2] (= (shape p1) (shape p2)))

(defmulti intersects? (fn [x y] [(class x) (class y)]))

(defmethod intersects? [Rectangle Rectangle]
  [r1 r2]
  (and (not (or (> (:x1 r1) (:x2 r2))
                (> (:x1 r2) (:x2 r1))))
       (not (or (> (:y1 r1) (:y2 r2))
                (> (:y1 r2) (:y2 r1))))))

(defmethod intersects? [MutableRectangleNode MutableRectangleNode]
  [r1 r2]
  (and (not (or (> (.getXMin r1) (.getXMax r2))
                (> (.getXMin r2) (.getXMax r1))))
       (not (or (> (.getYMin r1) (.getYMax r2))
                (> (.getYMin r2) (.getYMax r1))))))

(defmethod intersects? [MutableRectangleNode Rectangle]
  [r1 r2]
  (and (not (or (> (.getXMin r1) (:x2 r2))
                (> (:x1 r2) (.getXMax r1))))
       (not (or (> (.getYMin r1) (:y2 r2))
                (> (:y1 r2) (.getYMax r1))))))

(defmethod intersects? [Rectangle MutableRectangleNode]
  [r1 r2]
  (and (not (or (> (:x1 r1) (.getXMax r2))
                (> (.getXMin r2) (:x2 r1))))
       (not (or (> (:y1 r1) (.getYMax r2))
                (> (.getYMin r2) (:y2 r1))))))

(defmethod intersects? [Point Point] [p1 p2] (= (:shape p1) (:shape p2)))

(defmethod intersects? [Rectangle Point] [r p] (envelops? r p))

(defmethod intersects? [MutableRectangleNode Point] [rn p] (envelops? rn p))

(defmethod intersects? [Point Rectangle] [p r] (intersects? r p))

(defmethod intersects? [Point MutableRectangleNode] [p rn] (intersects? rn p))

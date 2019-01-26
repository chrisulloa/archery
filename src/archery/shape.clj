(ns archery.shape
  (:require [archery.util :refer [abs fast-contains? distinct-by fast-min-by fast-max-by]]
            [clojure.core.protocols :refer [Datafiable datafy]]))

(defprotocol TreeNode
  (leaf? [node] "Is this node a leaf?")
  (branch? [node] "Can this node have children?")
  (compress [node geometry] "Compress this node with a new geometry as child.")
  (add-child [node child] "Add a child to the node.")
  (best-child-for-insertion [node shape] "Find best child node to insert shape into.")
  (children-nodes [node] "Children nodes of the node.")
  (make-node [node children] "Makes new node from existing node and new children."))

(defprotocol Geometry
  (area-enlargement-diff [geom1 geom2] "Area of mbr around this shape and the other")
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
   (reduce minimum-bounding-rectangle (minimum-bounding-rectangle s1 s2) shapes)))

(defrecord RectangleNode [leaf? children x1 y1 x2 y2]
  Datafiable
  (datafy [_] {:type :RectangleNode,
               :leaf? leaf?,
               :shape [x1 y1 x2 y2],
               :children (mapv datafy children)})
  Geometry
  (area [_] (* (- x2 x1) (- y2 y1)))
  (area-enlargement-diff
    [_ geom]
    (let [[s-x1 s-y1 s-x2 s-y2] (rectangle-shape geom)]
      (- (* (- (max x2 s-x2) (min x1 s-x1))
            (- (max y2 s-y2) (min y1 s-y1)))
         (* (- x2 x1) (- y2 y1)))))
  (shape [_] [x1 y1 x2 y2])
  (rectangle-shape [_] [x1 y1 x2 y2])
  TreeNode
  (best-child-for-insertion
    [_ shape-to-insert]
    (loop [[node & rest-nodes] children
           best-node {:shape [], :area-diff ##Inf}]
      (if node
        (let [area-diff (area-enlargement-diff node shape-to-insert)]
          (if (zero? area-diff)
            (shape node)
            (if (< area-diff (:area-diff best-node))
              (recur rest-nodes {:shape (shape node), :area-diff area-diff})
              (recur rest-nodes best-node))))
        (:shape best-node))))
  (compress
    [rn geom]
    (let [new-children (if geom (conj children geom) children)]
      (if-not (empty? new-children)
        (->> new-children
             (apply minimum-bounding-rectangle)
             (shape)
             (apply (partial ->RectangleNode leaf? new-children)))
        rn)))
  (leaf? [_] leaf?)
  (branch? [_] true)
  (add-child [_ child]
    (->RectangleNode leaf? (conj children child) x1 y1 x2 y2))
  (children-nodes [_] (when-not leaf? children))
  (make-node [node new-children]
    (apply (partial ->RectangleNode leaf? new-children) (shape node))))

(defrecord RTree [root max-children min-children]
  Datafiable
  (datafy [_] {:type :RTree
               :root (datafy root),
               :max-children max-children,
               :min-children min-children}))

(defn rtree
  ([]
   (map->RTree
     {:root (->RectangleNode true [] 0 0 0 0), :max-children 4, :min-children 2}))
  ([params]
   (merge (rtree) params)))

(defmulti envelops? (fn [x y] [(class x) (class y)]))

(defmethod envelops? [Point Rectangle] [_ _] false)

(defmethod envelops? [Point RectangleNode] [_ _] false)

(defmethod envelops? [Rectangle Point]
  [r p]
  (and (<= (:x1 r) (:x p) (:x2 r))
       (<= (:y1 r) (:y p) (:y2 r))))

(defmethod envelops? [RectangleNode Point]
  [r p]
  (and (<= (:x1 r) (:x p) (:x2 r))
       (<= (:y1 r) (:y p) (:y2 r))))

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

(defmethod envelops? [Point Point]
  [p1 p2]
  (= (shape p1) (shape p2)))

(defmulti intersects?
          (fn [x y] [(class x) (class y)]))

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

(defmethod intersects? [Point Point]
  [p1 p2] (= (:shape p1) (:shape p2)))

(defmethod intersects? [Rectangle Point]
  [r p]
  (envelops? r p))

(defmethod intersects? [RectangleNode Point]
  [rn p]
  (envelops? rn p))

(defmethod intersects? [Point Rectangle]
  [p r] (intersects? r p))

(defmethod intersects? [Point RectangleNode]
  [p rn] (intersects? rn p))

(defn linear-seeds-across-dimensions
  [shapes]
  (let [x-max-lb (apply max-key (comp first rectangle-shape) shapes)
        x-min-ub (apply min-key (comp #(nth % 2) rectangle-shape) shapes)
        y-max-lb (apply max-key (comp second rectangle-shape) shapes)
        y-min-ub (apply min-key (comp last rectangle-shape) shapes)]
    [(if (= (shape x-max-lb) (shape x-min-ub))
       {:norm-separation ##Inf
        :seeds (take 2 (distinct-by shape shapes))}
       {:norm-separation (/ (- (-> x-max-lb rectangle-shape first)
                               (-> x-min-ub rectangle-shape (nth 2)))
                            (- (apply max (map (comp #(nth % 2) rectangle-shape) shapes))
                               (apply min (map (comp first rectangle-shape) shapes))))
        :seeds [x-max-lb x-min-ub]})
     (if (= (shape y-max-lb) (shape y-min-ub))
       {:norm-separation ##Inf
        :seeds (take 2 (distinct-by shape shapes))}
       {:norm-separation (/ (- (-> y-max-lb rectangle-shape second)
                               (-> y-min-ub rectangle-shape last))
                            (- (apply max (map (comp last rectangle-shape) shapes))
                               (apply min (map (comp second rectangle-shape) shapes))))
        :seeds [y-max-lb y-min-ub]})]))

(defn linear-seeds
  [shapes leaf?]
  (->> shapes
       (linear-seeds-across-dimensions)
       (fast-max-by :norm-separation ##Inf)
       (:seeds)
       (map #(apply (partial ->RectangleNode leaf? [%]) (rectangle-shape %)))))

(defn shape->seeds
  [shape r-seed l-seed]
  (if (<= (area-enlargement-diff r-seed shape)
          (area-enlargement-diff l-seed shape))
    [(compress r-seed shape) l-seed]
    [r-seed (compress l-seed shape)]))

(defn linear-split [rn min-children]
  (let [seeds (linear-seeds (:children rn) (leaf? rn))]
    (loop [r-seed (first seeds)
           l-seed (second seeds)
           shapes (remove #{(-> r-seed :children first)
                            (-> l-seed :children first)} (:children rn))]
      (if-not (empty? shapes)
        (cond
          (= min-children (+ (count (:children r-seed)) (count shapes)))
          (recur (reduce compress r-seed shapes)
                 l-seed
                 nil)
          (= min-children (+ (count (:children l-seed)) (count shapes)))
          (recur r-seed
                 (reduce compress l-seed shapes)
                 nil)
          :else
          (let [next-seeds (shape->seeds (first shapes) r-seed l-seed)]
            (recur (first next-seeds) (second next-seeds) (rest shapes))))
        [r-seed l-seed]))))
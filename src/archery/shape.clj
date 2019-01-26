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
    (let [new-children (if geom (conj children geom) children)
          new-node (fn [[x1 y1 x2 y2]]
                     (->RectangleNode leaf? new-children x1 y1 x2 y2))]
      (if-not (empty? new-children)
        (->> new-children
             (apply minimum-bounding-rectangle)
             (shape)
             (new-node))
        rn)))
  (leaf? [_] leaf?)
  (branch? [_] true)
  (add-child [_ child]
    (->RectangleNode leaf? (conj children child) x1 y1 x2 y2))
  (children-nodes [_] (when-not leaf? children))
  (make-node [_ new-children]
    (->RectangleNode leaf? new-children x1 y1 x2 y2)))

(defn rectangle-node
  [leaf? children [x1 y1 x2 y2]]
  (->RectangleNode leaf? children x1 y1 x2 y2))

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
  (loop [[geom & geoms] shapes
         splits {:x-max-lb ##-Inf, :x-min-ub ##Inf,
                 :x-min-lb ##Inf, :x-max-ub ##-Inf
                 :y-max-lb ##-Inf, :y-min-ub ##Inf
                 :y-min-lb ##Inf, :y-max-ub ##-Inf}]
    (if geom
      (recur
        geoms
        (let [rectangle (apply ->Rectangle (rectangle-shape geom))]
          (cond-> splits
                  (< (:x-max-lb splits) (:x1 rectangle))
                  (assoc :x-max-lb (:x1 rectangle) :x-max-lb-shape geom)
                  (> (:x-min-ub splits) (:x2 rectangle))
                  (assoc :x-min-ub (:x2 rectangle) :x-min-ub-shape geom)
                  (> (:x-min-lb splits) (:x1 rectangle))
                  (assoc :x-min-lb (:x1 rectangle))
                  (< (:x-max-ub splits) (:x2 rectangle))
                  (assoc :x-max-ub (:x2 rectangle))
                  (< (:y-max-lb splits) (:y1 rectangle))
                  (assoc :y-max-lb (:y1 rectangle) :y-max-lb-shape geom)
                  (> (:y-min-ub splits) (:y2 rectangle))
                  (assoc :y-min-ub (:y2 rectangle) :y-min-ub-shape geom)
                  (> (:y-min-lb splits) (:y1 rectangle))
                  (assoc :y-min-lb (:y1 rectangle))
                  (< (:y-max-ub splits) (:y2 rectangle))
                  (assoc :y-max-ub (:y2 rectangle)))))
      (let [{:keys [x-max-lb x-max-lb-shape
                    x-min-ub x-min-ub-shape
                    x-min-lb x-max-ub
                    y-max-lb y-max-lb-shape
                    y-min-ub y-min-ub-shape
                    y-min-lb y-max-ub]} splits]
        [(if (= x-max-lb-shape x-min-ub-shape)
           {:norm-separation ##Inf, :seeds (take 2 (distinct-by shape shapes))}
           {:norm-separation (/ (- x-max-lb x-min-ub) (- x-max-ub x-min-lb)),
            :seeds [x-max-lb-shape x-min-ub-shape]})
         (if (= y-max-lb-shape y-min-ub-shape)
           {:norm-separation ##Inf, :seeds (take 2 (distinct-by shape shapes))}
           {:norm-separation (/ (- y-max-lb y-min-ub) (- y-max-ub y-min-lb))
            :seeds [y-max-lb-shape y-min-ub-shape]})]))))

(defn linear-seeds
  [shapes leaf?]
  (->> shapes
       (linear-seeds-across-dimensions)
       (fast-max-by :norm-separation ##Inf)
       (:seeds)
       (map #(rectangle-node leaf? [%] (rectangle-shape %)))))

(defn shape->seeds
  [shape r-seed l-seed]
  (if (<= (area-enlargement-diff r-seed shape)
          (area-enlargement-diff l-seed shape))
    [(add-child r-seed shape) l-seed]
    [r-seed (add-child l-seed shape)]))

(defn linear-split [rn min-children]
  (let [seeds (linear-seeds (:children rn) (leaf? rn))]
    (loop [r-seed (first seeds)
           l-seed (second seeds)
           shapes (remove #{(-> r-seed :children first)
                            (-> l-seed :children first)} (:children rn))]
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
            (recur (first next-seeds) (second next-seeds) (rest shapes))))
        [(compress r-seed nil) (compress l-seed nil)]))))
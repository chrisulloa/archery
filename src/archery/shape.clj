(ns archery.shape
  (:require [archery.util :refer [abs fast-contains? distinct-by]]))

(defprotocol Geometry
  (dim [geom] "Dimension of the given geometry")
  (area [geom] "Area of the given geometry"))

(defrecord Rectangle [leaf? children shape]
  Geometry
  (dim [_] (count shape))
  (area [_]
    (apply * (map #(- (second %) (first %)) shape))))

(defrecord Point [shape]
  Geometry
  (dim [_] (count shape))
  (area [_] 0))

(defmulti envelops? (fn [x y] [(class x) (class y)]))

(defmethod envelops? [Point Rectangle] [_ _] false)

(defmethod envelops? [Rectangle Point]
  [r p]
  (letfn [(envelops-by-dim? [[[r-min r-max] p]]
            (<= r-min p r-max))]
    (->> (interleave (:shape r) (:shape p))
         (partition 2)
         (map envelops-by-dim?)
         (every? true?))))

(defmethod envelops? [Rectangle Rectangle]
  [r1 r2]
  (letfn [(envelops-by-dim? [[[r1-min r1-max] [r2-min r2-max]]]
            (<= r1-min r2-min r2-max r1-max))]
    (->> (interleave (:shape r1) (:shape r2))
         (partition 2)
         (map envelops-by-dim?)
         (every? true?))))

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

(defmethod intersects? [Point Point]
  [p1 p2] (= (:shape p1) (:shape p2)))

(defmethod intersects? [Rectangle Point]
  [r p]
  (envelops? r p))

(defmethod intersects? [Point Rectangle]
  [p r] (intersects? r p))

(defmulti collect-points class)

(defmethod collect-points Rectangle [geom]
  (:shape geom))

(defmethod collect-points Point [geom]
  (map vector (:shape geom)))

(defn minimum-bounding-rectangle
  "Given a shape or collection of shapes, computes the minimum
   bounding rectangle. collect-points must be defined for a shape."
  [& shapes]
  (->> shapes
       (map collect-points)
       (reduce #(map conj %1 %2))
       (map flatten)
       (map #(vector (reduce min %) (reduce max %)))
       (->Rectangle true nil)))

(defn shape->rectangle
  "Coerces a shape to a rectangle given its minimum boundary."
  [shape]
  (if (instance? Rectangle shape)
    shape
    (minimum-bounding-rectangle shape)))

(defn area-enlargement-diff
  "Difference in area of rectangle node before and after
   enlargement with a shape"
  [node shape]
  (- (area (minimum-bounding-rectangle shape node))
     (area node)))

(defn best-shape-for-insert
  [shapes shape-to-insert]
  (some->> shapes
           (map #(dissoc % :children))
           (map #(hash-map :node %
                           :diff (area-enlargement-diff % shape-to-insert)))
           (apply (partial min-key :diff))
           (:node)))

(defn compress-rectangle
  "Adjusts boundary for tight fit, after adding extra shapes if needed."
  ([rectangle]
   (if-let [children (:children rectangle)]
     (merge rectangle (apply minimum-bounding-rectangle children))
     rectangle))
  ([rectangle shape]
   (let [children (conj (:children rectangle) shape)]
     (merge rectangle
            (assoc (apply minimum-bounding-rectangle children) :children children))))
  ([rectangle shape & shapes]
   (reduce compress-rectangle (compress-rectangle rectangle shape) shapes)))

(defrecord RTree [tree dimension max-children min-children])

(defn rtree
  ([]
   (map->RTree
     {:tree (->Rectangle true nil []), :max-children 50, :min-children 1, :dimension 2}))
  ([params]
   (merge (rtree) params)))

;TODO: CLEAN UP THIS GOD AWFUL MESS
(defn augment-shape
  "Augments a shape by creating a map of sides along a dimension.
    e.g. (->Rectangle [[0 10] [5 15]]) => {0 [0 5] 1, [10 15]}
         (->Point [10 30]) => {0 [10 30], 1 [30 30}
   For use in calculating highest-low-side, lowest-high-side, etc."
  [shape]
  (assoc shape :augmented (zipmap (range (dim shape))
                                  (collect-points (shape->rectangle shape)))))

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
        (if (= (:shape max-lb) (:shape min-ub))
          (let [reduced-distinct-shapes (distinct-by :shape reduced-shapes)]
            {:dimension       d,
             :norm-separation ##Inf,
             :seeds           [(compress-rectangle (dissoc (first reduced-distinct-shapes) :augmented))
                               (compress-rectangle (dissoc (second reduced-distinct-shapes) :augmented))]})
          {:dimension       d
           :norm-separation (/ (- ((augmented-val first d) max-lb)
                                  ((augmented-val second d) min-ub))
                               (- ((augmented-val second d) max-ub)
                                  ((augmented-val first d) min-lb)))
           :seeds           [(compress-rectangle (dissoc min-ub :augmented))
                             (compress-rectangle (dissoc max-lb :augmented))]})))))

(defn linear-seeds
  [shapes]
  (->> shapes
       linear-seeds-across-dimensions
       (apply (partial max-key :norm-separation))
       (:seeds)))

(defn shape->seed
  [shape r-seed l-seed]
  (let [r-enlarged (compress-rectangle r-seed shape)
        l-enlarged (compress-rectangle l-seed shape)
        r-seed? (<= (- (area r-enlarged) (area r-seed))
                    (- (area l-enlarged) (area l-seed)))]
    (if r-seed?
      {:next-seed :r-seed, :enlarged-seed r-enlarged}
      {:next-seed :l-seed, :enlarged-seed l-enlarged})))

(defn initialize-seed
  "Creates a bounding box around a seed shape and includes it in vals."
  [seed leaf?]
  (-> seed
      minimum-bounding-rectangle
      (assoc :leaf? leaf? :children [seed])))

(defmulti linear-split class)

(defmethod linear-split Rectangle [r]
  ; TODO: Incorporate minimum m
  ; TODO: Resolve ties by entry count as well
  (when-let [shapes (:children r)]
    (let [seeds (linear-seeds shapes)]
      (loop [r-seed (initialize-seed (first seeds) (true? (:leaf? r)))
             l-seed (initialize-seed (second seeds) (true? (:leaf? r)))
             [shape & rest-shapes] (remove #{(-> r-seed :children first)
                                             (-> l-seed :children first)} shapes)]
        (if-not (nil? shape)
          (let [{:keys [next-seed enlarged-seed]} (shape->seed shape r-seed l-seed)]
            (if (= next-seed :r-seed)
              (recur enlarged-seed
                     l-seed
                     rest-shapes)
              (recur r-seed
                     enlarged-seed
                     rest-shapes)))
          (compress-rectangle (->Rectangle true nil [[0 0] [0 0]]) r-seed l-seed))))))

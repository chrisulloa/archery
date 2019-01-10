(ns rarbol.shape
  (:require [rarbol.util :refer [abs fast-contains?]]))

(defprotocol Geometry
  (dim [geom] "Dimension of the given geometry")
  (area [geom] "Area of the given geometry"))

(defrecord Rectangle [shape]
  Geometry
  (dim [_] (count shape))
  (area [_]
    (apply * (map #(- (second %) (first %)) shape))))

(defrecord Point [shape]
  Geometry
  (dim [_] (count shape))
  (area [_] 0))

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
  (->> (interleave (:shape r) (:shape p))
       (partition 2)
       (map #(fast-contains? (first %) (second %)))
       (some true?)
       (nil?)
       (not)))

(defmethod intersects? [Point Rectangle]
  [p r] (intersects? r p))

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

(defmethod envelops? [Point Point] [p1 p2] (= (:shape p1) (:shape p2)))

; Collects shape points along dimensions for calculating
; minimum bounding rectangle."
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
       (->Rectangle)))

(defn shape->rectangle
  "Coerces a shape to a rectangle given its minimum boundary."
  [shape]
  (if (instance? Rectangle shape)
    shape
    (minimum-bounding-rectangle shape)))

(defn augment-shape
  "Augments a shape by creating a map of sides along a dimension.
    e.g. (->Rectangle [[0 10] [5 15]]) => {0 [0 5] 1, [10 15]}
         (->Point [10 30]) => {0 [10 30], 1 [30 30}
   For use in calculating highest-low-side, lowest-high-side, etc."
  [shape]
  (assoc shape :augmented (zipmap (range (dim shape))
                                  (collect-points (shape->rectangle shape)))))

(defn augmented-key-getter
  "Retrieves the (first or second) position along a
   dimension of an augmented shape."
  [position dimension]
  (fn [shape] (-> shape :augmented (get dimension) position)))

(defn area-enlargement-diff
  "Difference in area of rectangle node before and after
   enlargement with a shape"
  [node shape]
  (- (area (minimum-bounding-rectangle shape node))
     (area node)))

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

(defn linear-seeds-across-dimensions
  [shapes]
  ;TODO: Make this easier to read
  (let [dimensions (dim (first shapes))
        reduced-shapes (map augment-shape shapes)
        min-or-max-side #(apply (partial %1 (augmented-key-getter %2 %3)) reduced-shapes)]
    (for [d (range dimensions)]
      (let [highest-low-side (min-or-max-side max-key first d)
            lowest-low-side (min-or-max-side min-key first d)
            highest-high-side (min-or-max-side max-key second d)
            lowest-high-side (min-or-max-side min-key second d)]
        {:dimension       d
         :seeds           [(compress-rectangle (dissoc lowest-high-side :augmented))
                           (compress-rectangle (dissoc highest-low-side :augmented))]
         :norm-separation (/ (- ((augmented-key-getter first d) highest-low-side)
                                ((augmented-key-getter second d) lowest-high-side))
                             (- ((augmented-key-getter second d) highest-high-side)
                                ((augmented-key-getter first d) lowest-low-side)))}))))

(defn linear-seeds
  "1.) Along each dimension, finds entry whose rectangle has the highest low side and
       the lowest high side, calculating the normalized separation for each dimension.
   2.)  Chooses the pair with the greatest normalized separation"
  [shapes]
  (->> shapes
       linear-seeds-across-dimensions
       (apply (partial max-key :norm-separation))
       (:seeds)))

(defn clean-seed
  "Removes any extra data attached to a shape during linear split."
  [seed]
  (dissoc seed :diff :next-seed :enlarged-seed))

(defn shape->seed-delta
  "For a given seed calculates the difference in area increase
   between potential insertion to the two seeds."
  [shape r-seed l-seed]
  (let [r-enlarged (compress-rectangle r-seed (clean-seed shape))
        r-area-diff (- (area r-enlarged) (area r-seed))
        l-enlarged (compress-rectangle l-seed (clean-seed shape))
        l-area-diff (- (area l-enlarged) (area l-seed))
        r-seed? (<= r-area-diff l-area-diff)]
    {:diff          (abs (- r-area-diff l-area-diff))
     :next-seed     (if r-seed? :r-seed :l-seed)
     :enlarged-seed (if r-seed? r-enlarged l-enlarged)}))

(defn next-picks
  [r-seed l-seed shapes]
  "This function is built for the PickNext algorithm, it sorts shapes
   by their potential seed enlargements and pre-calculates children for
   linear node split.
   1.) For each entry E not yet in a group, calculate area increase
       required for covering rectangle of group 1 and group 2.
   2.) Choose the entry with the maximum difference between area
        increases d1 and d2."
  (when-not (empty? shapes)
    (->> shapes
         (map (fn [shape]
                (merge shape
                       (shape->seed-delta shape r-seed l-seed))))
         (sort-by :diff >))))

(defn initialize-seed
  "Creates a bounding box around a seed shape and includes it in vals."
  [seed]
  (-> seed
      minimum-bounding-rectangle
      (assoc :leaf? true :children [seed])))

(defn initialized-shapes
  [init-r-seed init-l-seed shapes]
  (some->> shapes
           (remove #{(-> init-r-seed :children first)
                     (-> init-l-seed :children first)})
           (next-picks init-r-seed init-l-seed)))

(defmulti linear-split class)

(defmethod linear-split Rectangle [r]
  "1.) Apply algorithm PickSeeds to choose two entries to be first
       elements of the two new nodes.
   2.) If all groups have been assigned stop. If one group has so few
       entries that all the rest must be assigned to it for it to have
       the minimum m, assign them and stop.
   3.) Invoke PickNext to choose the next entry to assign. Add it to the
       group whose covering rectangle will have to be enlarged the least
       to accommodate it. Resolve ties by adding the entry to the group with
       the smaller area, then to the one with fewer entries, then to either.
       Repeat 2.)"
  ; TODO: Incorporate minimum m
  ; TODO: Resolve ties by entry count as well
  (when-let [shapes (:children r)]
    (let [seeds (linear-seeds shapes)]
      (loop [r-seed (initialize-seed (first seeds))
             l-seed (initialize-seed (second seeds))
             [shape & sorted-shapes] (initialized-shapes r-seed l-seed shapes)]
        (if-not (nil? shape)
          (let [{:keys [next-seed enlarged-seed]} shape]
            (if (= next-seed :r-seed)
              (recur enlarged-seed
                     l-seed
                     (next-picks enlarged-seed
                                 l-seed
                                 sorted-shapes))
              (recur r-seed
                     enlarged-seed
                     (next-picks r-seed
                                 enlarged-seed
                                 sorted-shapes))))
          (compress-rectangle (->Rectangle [[0 0] [0 0]]) r-seed l-seed))))))
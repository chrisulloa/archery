(ns rarbol.shape)

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

(defmulti envelops?
          (fn [x y] [(class x) (class y)]))

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

(defmulti collect-points class)

(defmethod collect-points Rectangle [geom] (:shape geom))

(defmethod collect-points Point [geom]
  (map vector (:shape geom)))

(defn minimum-bounding-rectangle
  [shapes]
  (->> shapes
       (map collect-points)
       (reduce #(map conj %1 %2))
       (map flatten)
       (map #(vector (reduce min %) (reduce max %)))
       (Rectangle.)))

(defn shape->rectangle
  [shape]
  (if (instance? Rectangle shape)
    shape
    (minimum-bounding-rectangle [shape])))

(defn augment-shape
  [shape]
  (assoc shape :augmented (zipmap (range (dim shape))
                                  (collect-points (shape->rectangle shape)))))

(defn augmented-shape-getter
  [position dimension]
  (fn [shape] (-> shape :augmented (get dimension) position)))

(defn enlargement
  [shape node]
  (- (area (minimum-bounding-rectangle [shape node]))
     (area node)))

(defn enlarge
  [rectangle shape]
  (let [new-values (conj (:values rectangle) shape)]
    (assoc (minimum-bounding-rectangle new-values)
      :values new-values)))

;TODO: Make this easier to read
(defn linear-seeds
  [shapes]
  (let [dimensions (dim (first shapes))
        reduced-shapes (map augment-shape shapes)
        key-val augmented-shape-getter
        min-or-max-side #(apply (partial %1 (key-val %2 %3)) reduced-shapes)]
    (apply (partial min-key :norm-separation)
           (for [d (range dimensions)]
             (let [highest-low-side (min-or-max-side max-key first d)
                   lowest-low-side (min-or-max-side min-key first d)
                   highest-high-side (min-or-max-side max-key second d)
                   lowest-high-side (min-or-max-side min-key second d)]
               {:dimension       d
                :seeds           [(dissoc lowest-high-side :augmented)
                                  (dissoc highest-low-side :augmented)]
                :norm-separation (/ (- ((key-val first d) highest-low-side)
                                             ((key-val second d) lowest-high-side))
                                    (- ((key-val second d) highest-high-side)
                                       ((key-val first d) lowest-low-side)))})))))

(defmulti linear-split class)

(defmethod linear-split Rectangle [r]
  (when-let [shapes (:values r)]
    (let [seeds (-> shapes linear-seeds :seeds)]
      (loop [shapes (remove #{(first seeds) (second seeds)} shapes)
             r-seed (-> [(first seeds)]
                        (minimum-bounding-rectangle)
                        (assoc :values [(first seeds)]))
             l-seed (-> [(second seeds)]
                        (minimum-bounding-rectangle)
                        (assoc :values [(second seeds)]))]
        (if (not (empty? shapes))
          ; TODO: Break ties using counts
          (if (<= (enlargement (first shapes) r-seed)
                  (enlargement (first shapes) l-seed))
            (recur (rest shapes) (enlarge r-seed (first shapes)) l-seed)
            (recur (rest shapes) r-seed (enlarge l-seed (first shapes))))
          [r-seed l-seed])))))
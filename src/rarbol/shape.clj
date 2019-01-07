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

(defn shape-with-split-data
  [shape]
  (let [points (collect-points shape)]
    {:shape shape
     :x-max (reduce max (:xs points))
     :y-max (reduce max (:ys points))
     :x-min (reduce min (:xs points))
     :y-min (reduce min (:xs points))}))
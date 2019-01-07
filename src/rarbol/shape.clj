(ns rarbol.shape)

(defprotocol Geometry
  (dim [geom] "Dimension of the given geometry")
  (area [geom] "Area of the given geometry"))

(defrecord Rectangle [points]
  Geometry
  (dim [_] (count points))
  (area [_]
    (* (map #(- (second %) (first %)) points))))

(defrecord Point [points]
  Geometry
  (dim [_] (count points))
  (area [_] 0))

(defmulti intersects?
          (fn [x y] [(class x) (class y)]))

(defmethod intersects? [Rectangle Rectangle]
  [r1 r2]
  (letfn [(intersects-by-dimension?
            [[r1p r2p]]
            (not (or (> (first r1p) (second r2p))
                     (> (first r2p) (second r1p)))))]
    (->> (interleave (:points r1) (:points r2))
         (partition 2)
         (map intersects-by-dimension?)
         (some true?))))

(defmulti envelops?
          (fn [x y] [(class x) (class y)]))

(defmethod envelops? [Rectangle Point]
  [r p]
  (and (<= (:x0 r) (:x p) (:x1 r))
       (<= (:y0 r) (:y p) (:y1 r))))

(defmethod envelops? [Rectangle Rectangle]
  [r1 r2]
  (and (<= (:x0 r1) (:x0 r2) (:x1 r2) (:x1 r1))
       (<= (:y0 r1) (:y0 r2) (:y1 r2) (:y1 r1))))

(defmulti collect-points class)

(defmethod collect-points Rectangle [shape]
  {:xs [(:x0 shape) (:x1 shape)]
   :ys [(:y0 shape) (:y1 shape)]})

(defmethod collect-points Point [shape]
  {:xs [(:x shape)]
   :ys [(:y shape)]})

(defn minimum-bounding-rectangle
  [shapes]
  (let [points (map collect-points shapes)
        xs (flatten (map :xs points))
        ys (flatten (map :ys points))]
    (Rectangle. (reduce min xs) (reduce max xs)
                (reduce min ys) (reduce max ys))))

(defn shape-with-split-data
  [shape]
  (let [points (collect-points shape)]
    {:shape shape
     :x-max (reduce max (:xs points))
     :y-max (reduce max (:ys points))
     :x-min (reduce min (:xs points))
     :y-min (reduce min (:xs points))}))


(defmulti linear-split class)

(defmethod linear-split Rectangle [r]
  (when (contains? :shapes r)
    (let [marked-shapes (map shape-with-split-data (:shapes r))
          highest-low-side-x (max-key :x-min marked-shapes)
          ;highest-high-side
          lowest-high-side-x (min-key :x-max marked-shapes)
          ;lowest-low-side
          highest-low-side-y (max-key :y-min marked-shapes)
          lowest-high-side-y (min-key :y-max marked-shapes)]


      )))


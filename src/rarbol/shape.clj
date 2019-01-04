(ns rarbol.shape)

(defprotocol Geometry
  (area [geom] "The area of the given geometry"))

(defrecord Rectangle [x0 x1 y0 y1]
  Geometry
  (area [_]
    (* (- x1 x0) (- y1 y0))))

(defrecord Point [x y]
  Geometry
  (area [_] 0))

(defmulti envelops?
          (fn [x y] [(class x) (class y)]))

(defmethod envelops? [Rectangle Point]
  [r p]
  (and (<= (:x0 r) (:x p) (:x1 r))
       (<= (:y0 r) (:y p) (:y1 r))))

(defmulti collect-points
          (fn [shapes]
            (let [shapes-type (set (map class shapes))]
              (when (= 1 (count shapes-type))
                (first shapes-type)))))

(defmethod collect-points Rectangle [shapes]
  {:xs (concat (map :x0 shapes)
               (map :x1 shapes))
   :ys (concat (map :y0 shapes)
               (map :y1 shapes))})

(defmethod collect-points Point [shapes]
  {:xs (map :x shapes)
   :ys (map :y shapes)})

(defn minimum-bounding-rectangle
  [children]
  (let [points (collect-points children)
        xs (:xs points)
        ys (:ys points)]
    (Rectangle. (reduce min xs) (reduce max xs)
                (reduce min ys) (reduce max ys))))

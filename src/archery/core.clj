(ns archery.core
  (:require [archery.shape :refer [->RTree map->RTree ->RectangleNode ->Point ->Rectangle]]
            [archery.visitor :refer [insert-visitor adjust-node-visitor enveloped-by-shape-visitor]]
            [archery.zipper :refer [zipper tree-inserter tree-visitor]]
            [clojure.core.protocols :refer [datafy]]
            [clojure.pprint :refer [pprint]])
  (:import [archery.shape RTree Rectangle RectangleNode Point]))

(defn rtree ^RTree
  ([] (map->RTree
        {:root (->RectangleNode true [] 0.0 0.0 0.0 0.0),
         :max-children 4,
         :min-children 2}))
  ([params] (merge (rtree) params)))

(defn insert ^RTree
  ([^RTree tree geom]
   (assoc tree :root (:node (tree-inserter
                              (zipper (:root tree))
                              [(insert-visitor geom)
                               (adjust-node-visitor (:min-children tree)
                                                    (:max-children tree))]))))
  ([tree geom & geoms]
   (reduce insert (insert tree geom) geoms)))

(defn search
  ([^RTree tree r]
     (:state (tree-visitor (zipper (:root tree)) [(enveloped-by-shape-visitor r)]))))

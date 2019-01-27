(ns archery.core
  (:require [archery.shape :refer [->RTree map->RTree ->RectangleNode ->Point ->Rectangle]]
            [archery.visitor :refer [insert-visitor adjust-node-visitor]]
            [archery.zipper :refer [zipper tree-inserter]]
            [clojure.core.protocols :refer [datafy]]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(defn rtree
  ([] (map->RTree
        {:root (->RectangleNode true [] 0.0 0.0 0.0 0.0),
         :max-children 4,
         :min-children 2}))
  ([params] (merge (rtree) params)))

(defn insert
  ([tree geom]
   (assoc tree :root (:node (tree-inserter
                              (zipper (:root tree))
                              [(insert-visitor geom)
                               (adjust-node-visitor (:min-children tree)
                                                    (:max-children tree))]))))
  ([tree geom & geoms]
    (reduce insert (insert tree geom) geoms)))

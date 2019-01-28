(ns archery.core
  (:require [archery.shape :refer [->RTree map->RTree ->RectangleNode ->Point ->Rectangle]]
            [archery.visitor :refer [insert-visitor adjust-node-visitor enveloped-by-shape-visitor]]
            [archery.zipper :refer [zipper tree-inserter tree-visitor]]
            [archery.quadratic-node-split :refer [quadratic-split]]
            [archery.linear-node-split :refer [linear-split]]
            [clojure.core.protocols :refer [datafy]]
            [clojure.pprint :refer [pprint]])
  (:import [archery.shape Rectangle RectangleNode Point])
  (:gen-class))

(defn rtree
  ([] (map->RTree
        {:root (->RectangleNode true [] 0.0 0.0 0.0 0.0),
         :max-children 4,
         :min-children 2,
         :node-split :quadratic}))
  ([params] (merge (rtree) params)))

(defn insert
  ([tree geom]
   (let [visitors [(insert-visitor geom)
                   (adjust-node-visitor
                     (:min-children tree)
                     (:max-children tree)
                     (if (= :quadratic (:node-split tree))
                       quadratic-split
                       linear-split))]]
     (assoc tree :root (:node (tree-inserter (zipper (:root tree)) visitors)))))
  ([tree geom & geoms]
   (reduce insert (insert tree geom) geoms)))

(defn search
  ([tree r]
   (:state (tree-visitor (zipper (:root tree)) [(enveloped-by-shape-visitor r)]))))


(ns archery.core
  (:require [archery.shape :refer [->RectangleNode ->Point ->Rectangle]]
            [archery.visitor :refer [insert-visitor adjust-node-visitor enveloped-by-shape-visitor]]
            [archery.zipper :refer [zipper tree-inserter tree-visitor]]
            [archery.quadratic-node-split :refer [quadratic-split]]
            [archery.linear-node-split :refer [linear-split]]
            [clojure.core.protocols :refer [Datafiable datafy]]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(defprotocol Tree
  (insert [tree geom] "Insert value into rtree.")
  (search [tree geom] "Search RTree for values contained in geom."))

(defrecord RTree [root max-children min-children node-split]
  Datafiable
  (datafy [_] {:type :RTree,
               :max-children max-children,
               :min-children min-children,
               :node-split node-split
               :root (datafy root)})
  Tree
  (insert [tree geom]
    (let [visitors [(insert-visitor geom)
                    (adjust-node-visitor min-children max-children node-split)]]
      (assoc tree :root (:node (tree-inserter (zipper root) visitors)))))
  (search [_ geom]
    (:state (tree-visitor (zipper root) [(enveloped-by-shape-visitor geom)]))))

(defn rtree
  ([] (map->RTree
        {:root (->RectangleNode true [] 0.0 0.0 0.0 0.0),
         :max-children 4,
         :min-children 2,
         :node-split quadratic-split}))
  ([params] (merge (rtree) params)))

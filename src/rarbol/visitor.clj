(ns rarbol.visitor
  (:require [rarbol.zipper :refer [visitor zipper]]
            [rarbol.shape :refer [envelops?]]
            [rarbol.util :refer [fast-contains?]])
  (:import [rarbol.shape Point]))

(defn leaf-visitor
  "Visitor that collects all leaf nodes."
  [node state]
  (when (:leaf? node)
    {:state (conj state node)}))

(defn shape-visitor
  "Visitor that returns node which contains shape."
  [shape]
  (fn [node state]
    (when (and (:leaf? node)
               (fast-contains? (:children node) shape))
      {:state (conj state node)
       :stop  true})))

(defn unenveloping-node-visitor
  "Visitor that skips nodes which do not contain shape."
  [shape]
  (fn [node state]
    (when-not (or (type Point) (envelops? node shape))
      {:next true})))

(defn leaf-collector
  "Collects all leaf nodes."
  [node]
  (:state (visitor (zipper node) #{} [leaf-visitor])))

(defn shape-finder
  "Finds first node that contains the shape."
  [node shape]
  (:state
    (visitor
      (zipper node) #{} [(unenveloping-node-visitor shape)
                         (shape-visitor shape)])))

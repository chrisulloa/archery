(ns rarbol.visitor
  (:require [rarbol.zipper :refer [visitor zipper]]
            [rarbol.shape :refer [envelops? intersects?]]
            [rarbol.util :refer [fast-contains?]])
  (:import [rarbol.shape Point]))

(defn leaf-visitor
  "Visitor that collects all leaf nodes."
  [node state]
  (when (:leaf? node)
    {:state (conj state node)}))

(defn enveloping-node-visitor
  "Visitor that returns node which contains shape."
  [shape]
  (fn [node state]
    (when (and (:leaf? node)
               (fast-contains? (:children node) shape))
      {:state (conj state node)
       :stop  true})))

(defn non-enveloping-node-visitor
  "Visitor that skips nodes which do not contain shape."
  [shape]
  (fn [node state]
    (when-not (or (type Point) (envelops? node shape))
      {:next true})))

(defn non-intersecting-visitor
  [rectangle]
  (fn [node state]
    (when-not (or (type Point) (intersects? node rectangle))
      {:next true})))

(defn enveloped-points-visitor
  [rectangle]
  (fn [node state]
    (when (and (:leaf? node))
      (->> node
           :children
           (filter #(envelops? rectangle %))
           (conj state)
           (hash-map :state)))))

(defn leaf-collector
  "Collect all leaf nodes."
  [node]
  (:state (visitor (zipper node) #{} [leaf-visitor])))

(defn enveloping-node-finder
  "Finds first node that contains the shape."
  [node shape]
  (first
    (:state
      (visitor
        (zipper node) #{} [(non-enveloping-node-visitor shape)
                           (enveloping-node-visitor shape)]))))

(defn rectangle-contains-collector
  "Find entries which are enveloped by given rectangle."
  [node rectangle]
  (:state
    (visitor
      (zipper node) #{} [(non-intersecting-visitor rectangle)
                         (enveloped-points-visitor rectangle)])))

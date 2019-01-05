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

(defn enveloped-shape-visitor
  "Visitor that returns node which contains shape."
  [shape]
  (fn [node state]
    (when (and (:leaf? node)
               (fast-contains? (:children node) shape))
      {:state (conj state node)
       :stop  true})))

(defn non-enveloped-shape-visitor
  "Visitor that skips nodes which do not contain shape."
  [shape]
  (fn [node state]
    (when-not (or (type Point) (envelops? node shape))
      {:next true})))

(defn non-intersected-visitor
  [rectangle]
  (fn [node state]
    (when-not (or (type Point) (intersects? node rectangle))
      {:next true})))

(defn enveloped-children-visitor
  [rectangle]
  (fn [node state]
    (when (:leaf? node)
      (some->> node
               :children
               (filter #(envelops? rectangle %))
               (concat state)
               (hash-map :state)))))

(defn leaf-collector
  "Collect all leaf nodes."
  [node]
  (:state (visitor (zipper node) #{} [leaf-visitor])))

(defn enveloped-shape-finder
  "Finds first node that contains the shape."
  [node shape]
  (first
    (:state
      (visitor
        (zipper node) #{} [(non-enveloped-shape-visitor shape)
                           (enveloped-shape-visitor shape)]))))

(defn enveloped-children-collector
  "Find entries which are enveloped by given rectangle."
  [node rectangle]
  (:state
    (visitor
      (zipper node) #{} [(non-intersected-visitor rectangle)
                         (enveloped-children-visitor rectangle)])))

(ns rarbol.visitor
  (:require [rarbol.zipper :refer [visitor zipper]]
            [rarbol.shape :refer [area envelops? intersects? minimum-bounding-rectangle]]
            [rarbol.util :refer [fast-contains?]])
  (:import [rarbol.shape Point Rectangle]))

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
               (fast-contains? (:shapes node) shape))
      {:state (conj state node)
       :stop  true})))

(defn non-enveloped-shape-visitor
  "Visitor that skips nodes which do not contain shape."
  [shape]
  (fn [node state]
    (when-not (envelops? node shape)
      {:next true})))

(defn non-intersected-visitor
  [rectangle]
  (fn [node state]
    (when-not (intersects? node rectangle)
      {:next true})))

(defn enveloped-shapes-visitor
  [rectangle]
  (fn [node state]
    (when (:leaf? node)
      (some->> node
               :shapes
               (filter #(envelops? rectangle %))
               (concat state)
               (hash-map :state)))))

(defn insertion-visitor
  [shape]
  (letfn [(enlargement [node]
            (- (area (minimum-bounding-rectangle [shape node]))
               (area node)))]
    (fn [node state]
      (when (:leaf? node)
        {:state node
         :stop  true}
        (if (and (not (empty? state))
                 (<= (enlargement state) (enlargement node)))
          {:next true}
          {:state node})))))

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

(defn enveloped-shapes-collector
  "Find entries which are enveloped by given rectangle."
  [node rectangle]
  (:state
    (visitor
      (zipper node) #{} [(non-intersected-visitor rectangle)
                         (enveloped-shapes-visitor rectangle)])))

(defn insertion-finder
  "Finds node that is best suited for insertion of shape."
  [node shape]
  (:state (visitor (zipper node) #{} [(insertion-visitor shape)])))

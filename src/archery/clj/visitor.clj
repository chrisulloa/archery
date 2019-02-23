(ns archery.clj.visitor
  (:require [archery.clj.zipper :refer [tree-visitor tree-inserter zipper]]
            [archery.clj.shape :refer [leaf? compress envelops? intersects? shape
                                       add-child choose-child-for-insert children]]
            [archery.clj.util :refer [fast-contains?]])
  (:import (archery.clj.shape Rectangle Point)
           (archery.util MutableRectangleNode)))

(defn leaf-visitor
  "Visitor that collects all leaf nodes."
  [node state]
  (when (leaf? node)
    {:state (conj state node)
     :next true}))

(defn node-contains-shape-visitor
  "Visitor that returns node which contains shape."
  [shape]
  (fn [node _]
    (when (and (leaf? node)
               (envelops? node shape)
               (fast-contains? (children node) shape))
      {:state node
       :stop true})))

(defn enveloped-by-shape-visitor
  "Visitor that returns shapes enveloped by given rectangle."
  [rectangle]
  (fn [node state]
    (if (or (envelops? node rectangle)
            (intersects? node rectangle))
      (when (leaf? node)
        {:state
         (->> node
              .getChildren
              (filter #(envelops? rectangle %))
              (concat state))
         :next true})
      {:next true})))

(defn collect-shapes-visitor
  [node state]
  (if-not (leaf? node)
    {:state (update state :nodes #(conj % node))}
    (let [children (children node)]
      {:state {:nodes (conj (:nodes state) node)
               :rectangles (concat (:rectangles state)
                                   (filter #(= (class %) Rectangle) children))
               :points (concat (:points state)
                               (filter #(= (class %) Point) children))}})))

(defn shapes-collector
  "Collects all points"
  [node]
  (:state (tree-visitor (zipper node) {:nodes []} [collect-shapes-visitor])))

(defn leaf-collector
  "Collect all leaf nodes."
  [node]
  (:state (tree-visitor (zipper node) [leaf-visitor])))

(defn node-contains-shape-finder
  "Finds first node that contains the shape."
  [node shape]
  (:state
    (tree-visitor
      (zipper node) [(node-contains-shape-visitor shape)])))

(defn adjust-node-visitor
  [min-children ^long max-children split-fn]
  (fn [node {:keys [inserted? child-split? enlarged-node?]}]
    (when inserted?
      (if (< max-children (count (children node)))
        {:node (split-fn node min-children),
         :child-split? true}
        (if (or child-split? enlarged-node?)
          {:node [(compress node)]}
          {:node [node], :stop true})))))

(defn insert-visitor
  [shape-to-insert]
  (fn [node {:keys [inserted? next-node]}]
    (when-not inserted?
      (let [found-best-shape? (= node next-node)]
        (if (or found-best-shape? (nil? next-node))
          (if (leaf? node)
            {:node (add-child node shape-to-insert),
             :state {:inserted? true,
                     :enlarged-node? (not (envelops? node shape-to-insert))}}
            {:state {:next-node (choose-child-for-insert node shape-to-insert),
                     :move-down? found-best-shape?},
             :next true})
          {:next true, :state {:move-down? false}})))))

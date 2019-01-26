(ns archery.visitor
  (:require [archery.zipper :refer [tree-visitor tree-inserter zipper]]
            [archery.shape :refer [leaf? compress linear-split
                                   envelops? intersects? shape
                                   add-child best-child-for-insertion ->RTree]]
            [archery.util :refer [fast-contains?]])
  (:import [archery.shape Rectangle Point]))

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
               (fast-contains? (:children node) shape))
      {:state node
       :stop true})))

(defn enveloped-shapes-visitor
  "Visitor that returns shapes enveloped by given rectangle."
  [rectangle]
  (fn [node state]
    (if (or (envelops? node rectangle)
            (intersects? node rectangle))
      (when (leaf? node)
        {:state
         (->> node
              :children
              (filter #(envelops? rectangle %))
              (concat state))
         :next true})
      {:next true})))

(defn collect-shapes-visitor
  [node state]
  (if-not (leaf? node)
    {:state (update state :nodes #(conj % node))}
    (let [children (:children node)]
      {:state {:nodes (:nodes state)
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

(defn enveloped-shapes-collector
  "Find entries which are enveloped by given rectangle."
  [node rectangle]
  (:state
    (tree-visitor
      (zipper node) [(enveloped-shapes-visitor rectangle)])))

(defn adjust-node-visitor
  [min-children max-children]
  (fn [node state]
    (when (:inserted? state)
      (if (< max-children (count (:children node)))
        {:node (linear-split node min-children),
         :child-split? true}
        (if (or (:child-split? state) (:enlarged-node? state))
          {:node [(compress node nil)]}
          {:node [node], :stop true})))))

(defn insert-visitor
  [shape-to-insert]
  (fn [node state]
    (when-not (:inserted? state)
      (let [found-best-shape? (= (shape node) (:next-node state))]
        (if (or (not (:next-node state)) found-best-shape?)
          (if (leaf? node)
            {:node (add-child node shape-to-insert),
             :state {:inserted? true,
                     :enlarged-node? (not (envelops? node shape-to-insert))}}
            {:state {:next-node (best-child-for-insertion node shape-to-insert),
                     :move-down? found-best-shape?},
             :next true})
          {:next true, :state {:move-down? false}})))))

(defn insert
  [tree geoms]
  (let [root (:root tree)
        min-children (:min-children tree)
        max-children (:max-children tree)]
    (loop [[geom & rest-geoms] geoms
           root root]
      (if geom
        (recur rest-geoms
               (:node (tree-inserter
                        (zipper root)
                        [(insert-visitor geom)
                         (adjust-node-visitor min-children max-children)])))
        (->RTree root min-children max-children)))))

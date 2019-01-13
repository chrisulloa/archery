(ns archery.visitor
  (:require [archery.zipper :refer [tree-visitor tree-inserter zipper]]
            [archery.shape :refer [area-enlargement-diff
                                  best-shape-for-insert
                                  linear-split
                                  compress-rectangle
                                  area
                                  envelops?
                                  intersects?]]
            [archery.util :refer [fast-contains?]]))

(defn leaf-visitor
  "Visitor that collects all leaf nodes."
  [node state]
  (when (:leaf? node)
    {:state (conj state node)
     :next  true}))

(defn node-contains-shape-visitor
  "Visitor that returns node which contains shape."
  [shape]
  (fn [node state]
    (if (envelops? node shape)
      (when (and (:leaf? node)
                 (fast-contains? (:children node) shape))
        {:state (conj state node)
         :stop  true})
      {:next true})))

(defn enveloped-shapes-visitor
  "Visitor that returns shapes enveloped by given rectangle."
  [rectangle]
  (fn [node state]
    (if (or (envelops? node rectangle)
            (intersects? node rectangle))
      (when (:leaf? node)
        {:state
               (->> node
                    :children
                    (filter #(envelops? rectangle %))
                    (concat state))
         :next true})
      {:next true})))

(defn leaf-collector
  "Collect all leaf nodes."
  [node]
  (:state (tree-visitor (zipper node) [leaf-visitor])))

(defn node-contains-shape-finder
  "Finds first node that contains the shape."
  [node shape]
  (first
    (:state
      (tree-visitor
        (zipper node) [(node-contains-shape-visitor shape)]))))

(defn enveloped-shapes-collector
  "Find entries which are enveloped by given rectangle."
  [node rectangle]
  (:state
    (tree-visitor
      (zipper node) [(enveloped-shapes-visitor rectangle)])))

(defn adjust-node-visitor
  ([] (adjust-node-visitor nil))
  ([max-children]
   (fn [node state]
     (when (:inserted? state)
       {:node (if (< (or max-children 50) (count (:children node)))
                (linear-split (compress-rectangle node))
                (compress-rectangle node))}))))

(defn insert-visitor
  ([shape]
   (insert-visitor shape nil))
  ([shape max-children]
   (fn [node state]
     (when-not (:inserted? state)
       (if (or (nil? (:next-node state))
               (= (:shape node) (:shape (:next-node state))))
         (if (:leaf? node)
           {:node  (if (<= (or max-children 50) (count (:children node)))
                     (linear-split (compress-rectangle node shape))
                     (compress-rectangle node shape))
            :state {:inserted? true},
            :next  true}
           {:state {:next-node (best-shape-for-insert (:children node) shape)}})
         {:next true})))))

(defn insert
  ([tree shape]
   (let [max-children (:max-children tree)]
     (update tree :tree #(:node (tree-inserter (zipper %)
                                               [(insert-visitor shape max-children)
                                                (adjust-node-visitor max-children)])))))
  ([tree shape & shapes]
   (reduce insert (insert tree shape) shapes)))
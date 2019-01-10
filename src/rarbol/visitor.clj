(ns rarbol.visitor
  (:require [rarbol.zipper :refer [tree-visitor tree-inserter zipper]]
            [rarbol.shape :refer [area-enlargement-diff
                                  best-shape-for-insert
                                  linear-split
                                  compress-rectangle
                                  area
                                  envelops?
                                  intersects?]]
            [rarbol.util :refer [fast-contains?]]))

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

(defn insertion-visitor
  "Visitor that returns the best node for insertion of given shape."
  [shape]
  (fn [node state]
    (when (:leaf? node)
      {:state node
       :stop  true}
      (if (and (not (empty? state))
               (<= (area-enlargement-diff state shape)
                   (area-enlargement-diff node shape)))
        {:next true}
        {:state node}))))

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

(defn insertion-finder
  "Finds node that is best suited for insertion of shape."
  [node shape]
  (:state (tree-visitor (zipper node) [(insertion-visitor shape)])))

(defn adjust-node-visitor
  [node state]
  (when (:inserted? state)
    {:node (compress-rectangle node)}))

(defn insert-visitor
  ([shape]
   (insert-visitor shape nil))
  ([shape max-children]
   (fn [node state]
     (when-not (:inserted? state)
       (if (or (nil? (:next-node state))
               (= node (:next-node state)))
         (if (:leaf? node)
           {:node  (if (<= (or max-children 50) (count (:children node)))
                     (linear-split (compress-rectangle node shape))
                     (compress-rectangle node shape))
            :state {:inserted? true},
            :next  true}
           {:next  false
            :state {:next-node (best-shape-for-insert (:children node) shape)}})
         {:next true})))))

(defn insert
  ([node shape]
   (:node (tree-inserter (zipper node) [(insert-visitor shape 2)
                                        adjust-node-visitor])))
  ([node shape & shapes]
   (reduce insert (insert node shape) shapes)))

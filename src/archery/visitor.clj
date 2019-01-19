(ns archery.visitor
  (:require [archery.zipper :refer [tree-visitor tree-inserter zipper]]
            [archery.shape :refer :all]
            [archery.util :refer [fast-contains?]]))

(defn leaf-visitor
  "Visitor that collects all leaf nodes."
  [node state]
  (when (leaf? node)
    {:state (conj state node)
     :next  true}))

(defn node-contains-shape-visitor
  "Visitor that returns node which contains shape."
  [shape]
  (fn [node state]
    (when (and (leaf? node)
               (envelops? node shape)
               (fast-contains? (children node) shape))
      {:state node
       :stop  true})))

(defn enveloped-shapes-visitor
  "Visitor that returns shapes enveloped by given rectangle."
  [rectangle]
  (fn [node state]
    (if (or (envelops? node rectangle)
            (intersects? node rectangle))
      (when (leaf? node)
        {:state
               (->> node
                    children
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
  ([] (adjust-node-visitor nil))
  ([max-children]
   (fn [node state]
     (when (:inserted? state)
       {:node (if (< (or max-children 50) (count (.-children node)))
                (linear-split (compress-node node))
                (compress-node node))}))))

(defn insert-visitor
  ([shape]
   (insert-visitor shape nil))
  ([shape-to-insert max-children]
   (fn [node state]
     (when-not (:inserted? state)
       (if (or (nil? (:next-node state))
               (= (.-shape node) (:next-node state)))
         (if (leaf? node)
           {:node  (if (<= (or max-children 50) (count (.-children node)))
                     (linear-split (compress-node node shape-to-insert))
                     (compress-node node shape-to-insert))
            :state {:inserted? true},
            :next  true}
           {:state {:next-node (best-node-for-insertion (.-children node)
                                                        shape-to-insert)}})
         {:next true})))))

(defn insert
  ([tree shape]
   (let [max-children (:max-children tree)]
     (update tree :root #(:node (tree-inserter (zipper %)
                                               [(insert-visitor shape max-children)
                                                (adjust-node-visitor max-children)])))))
  ([tree shape & shapes]
   (reduce insert (insert tree shape) shapes)))
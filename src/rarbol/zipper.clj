(ns rarbol.zipper
  (:require [clojure.zip :as zip]
            [rarbol.shape :refer [minimum-bounding-rectangle
                                  area-enlargement-diff
                                  linear-split
                                  compress-rectangle
                                  ->Rectangle]])
  (:import [rarbol.shape Rectangle]))

(defmulti branch? class)

(defmethod branch? :default [_] false)

(defmethod branch? Rectangle [r] true)

(defmulti children class)

(defmethod children Rectangle [r] (:children r))

(defmulti make-node (fn [node children] (class node)))

(defmethod make-node Rectangle [r children]
  (assoc (apply compress-rectangle r children) :children children))

(defn zipper [node]
  (zip/zipper branch? children make-node node))

(defn visit-node
  [start-node start-state visitors]
  (loop [node start-node
         state start-state
         [first-visitor & rest-visitors] visitors]
    (let [context (merge {:node node, :state state, :stop false, :next false}
                         (first-visitor node state))
          {new-node  :node
           new-state :state
           stop      :stop
           next      :next} context]
      (if (or next stop (nil? rest-visitors))
        {:node new-node, :state new-state, :stop stop}
        (recur new-node new-state rest-visitors)))))

(defn tree-visitor
  ([zipper visitors]
   (tree-visitor zipper nil visitors))
  ([zipper initial-state visitors]
   (loop [loc zipper
          state initial-state]
     (let [context (visit-node (zip/node loc) state visitors)
           new-node (:node context)
           new-state (:state context)
           stop (:stop context)
           new-loc (if (= new-node (zip/node loc))
                     loc
                     (zip/replace loc new-node))
           next-loc (zip/next new-loc)]
       (if (or (zip/end? next-loc) (= stop true))
         {:node (zip/root new-loc) :state new-state}
         (recur next-loc new-state))))))

(defn min-node
  [nodes shape]
  (some->> nodes
           (map #(hash-map :node % :diff (area-enlargement-diff % shape)))
           (apply (partial min-key :diff))
           (:node)))

(defn insert-visitor
  ([shape]
   (insert-visitor shape nil))
  ([shape m]
   (fn [node state]
     (let [max-children (or m 50)]
       (when-not (:inserted? state)
         (if (or (nil? (:next-node state))
                 (= node (:next-node state)))
           (if (:leaf? node)
             {:node  (if (<= max-children (count (:children node)))
                       (linear-split (compress-rectangle node shape))
                       (compress-rectangle node shape))
              :state {:inserted? true},
              :next  true}
             {:next  false
              :state {:next-node (min-node (:children node) shape)}})
           {:next true}))))))

(defn tree-inserter
  ([zipper visitors]
   (tree-inserter zipper nil visitors))
  ([zipper initial-state visitors]
   (loop [loc zipper
          state initial-state]
     (let [context (visit-node (zip/node loc) state visitors)
           new-node (:node context)
           new-state (:state context)
           stop (:stop context)
           new-loc (if (= new-node (zip/node loc))
                     loc
                     (zip/replace loc new-node))
           next-loc (if (:inserted? new-state)
                      (zip/up new-loc)
                      (zip/next new-loc))]
       (if (or (nil? next-loc) (zip/end? next-loc) (= stop true))
         {:node (zip/root new-loc), :state new-state}
         (recur next-loc new-state))))))

(defn insert
  ([node shape]
   (:node (tree-inserter (zipper node) [(insert-visitor shape 1)])))
  ([node shape & shapes]
    (reduce insert (insert node shape) shapes)))

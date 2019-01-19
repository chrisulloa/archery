(ns archery.zipper
  (:require [clojure.zip :as zip]
            [archery.shape :refer :all]))

(defn zipper [node]
  (zip/zipper branch? children-nodes make-node node))

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
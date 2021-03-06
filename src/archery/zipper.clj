(ns archery.zipper
  (:require [clojure.zip :as zip]
            [archery.shape :refer [map->RectangleNode branch?
                                   children-nodes make-node compress]]))

(defn zipper [node] (zip/zipper branch? children-nodes make-node node))

(defn replace-node
  ([loc node]
   (zip/replace loc node))
  ([loc node1 node2]
   (let [[_ {r :r :as path}] loc
         zero 0.0]
     (if (nil? path)
       (with-meta [(compress
                     (map->RectangleNode {:leaf? false,
                                          :children [node1 node2],
                                          :x1 zero, :y1 zero, :x2 zero, :y2 zero}))
                   (assoc path :changed? true)]
                  (meta loc))
       (with-meta [node1 (assoc path :r (cons node2 r) :changed? true)]
                  (meta loc))))))

(defn visit-node
  [start-node start-state visitors]
  (loop [node start-node
         state start-state
         [first-visitor & rest-visitors] visitors]
    (let [context (merge {:node node, :state state, :stop false, :next false}
                         (first-visitor node state))
          {new-node :node
           new-state :state
           stop :stop
           next :next} context]
      (if (or next stop (nil? rest-visitors))
        {:node new-node, :state new-state, :stop stop}
        (recur new-node new-state rest-visitors)))))

(defn tree-visitor
  ([zipper visitors]
   (tree-visitor zipper nil visitors))
  ([zipper initial-state visitors]
   (loop [loc zipper
          state initial-state]
     (let [{new-node :node
            new-state :state
            stop :stop} (visit-node (zip/node loc) state visitors)
           new-loc (if (= new-node (zip/node loc))
                     loc
                     (zip/replace loc new-node))
           next-loc (zip/next new-loc)]
       (if (or (zip/end? next-loc) stop)
         {:node (zip/root new-loc) :state new-state}
         (recur next-loc new-state))))))

(defn tree-inserter
  ([zipper visitors]
   (tree-inserter zipper nil visitors))
  ([zipper initial-state visitors]
   (loop [loc zipper
          state initial-state]
     (let [{new-node :node
            new-state :state
            stop :stop} (visit-node (zip/node loc) state visitors)
           new-loc (if (= new-node (zip/node loc))
                     loc
                     (apply replace-node loc new-node))
           next-loc (if (:inserted? new-state)
                      (zip/up new-loc)
                      (if (:move-down? new-state)
                        (zip/down new-loc)
                        (zip/next new-loc)))]
       (if (or stop (nil? next-loc) (zip/end? next-loc))
         {:node (zip/root new-loc), :state new-state}
         (recur next-loc new-state))))))
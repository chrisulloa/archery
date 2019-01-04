(ns rarbol.zipper
  (:require [clojure.zip :as zip]
            [rarbol.shape :refer [minimum-bounding-rectangle ->Rectangle]])
  (:import [rarbol.shape Rectangle]))

(defmulti branch? class)

(defmethod branch? :default [_] false)

(defmethod branch? Rectangle [r] true)

(defmulti children class)

(defmethod children Rectangle [r] (:children r))

(defmulti make-node (fn [node children] (class node)))

(defmethod make-node Rectangle [r children]
  (let [bounds (minimum-bounding-rectangle children)]
    (assoc (apply ->Rectangle bounds) :children children)))

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

(defn visitor
  ([zipper visitors]
   (visitor zipper nil visitors))
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

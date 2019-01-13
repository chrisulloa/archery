(ns rarbol.core
  (:require [rarbol.shape :refer :all]
            [rarbol.zipper :refer :all]
            [rarbol.visitor :refer :all]
            [rarbol.util :refer :all])
  (:import (com.github.davidmoten.rtree RTree)
           (com.github.davidmoten.rtree.geometry Geometries)
           (com.github.davidmoten.grumpy.core Position))
  (:gen-class))

(defn insert
  ([rt shape]
   (update rt :tree #(tree-insert % shape)))
  ([rt shape & shapes]
   (reduce insert (insert rt shape) shapes)))

(defn -main []
  (println "Starting benchmark: Inserting 100 rectangles into RTree {:max-children 50}")
  (let [random-shapes (fn [] (let [min-x (rand-int 500000)
                                      max-x (+ min-x (rand-int 100000))
                                      min-y (rand-int 500000)
                                      max-y (+ min-y (rand-int 100000))]
                                  [[min-x max-x] [min-y max-y]]))
        sample (take 10000 (repeatedly random-shapes))
        create-rectangle (fn [[[min-x max-x] [min-y max-y]]]
                           (Geometries/rectangle (double min-x)
                                                 (double min-y)
                                                 (double max-x)
                                                 (double max-y)))]
    (dotimes [n 10]
      (println (format "Clojure RTree Iteration %s" n))
      (time (apply insert (rtree) (map ->Rectangle sample))))
    (dotimes [n 10]
      (println (format "Java RTree Iteration %s" n))
      (time (reduce #(.add %1 nil %2) (RTree/create) (map create-rectangle sample))))))

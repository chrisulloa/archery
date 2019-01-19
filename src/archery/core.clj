(ns archery.core
  (:require [archery.shape :refer :all]
            [archery.zipper :refer :all]
            [archery.visitor :refer :all]
            [archery.util :refer :all])
  (:import (com.github.davidmoten.rtree RTree)
           (com.github.davidmoten.rtree.geometry Geometries))
  (:gen-class))

(defn -main []
  (println "Starting benchmark: Inserting 10,000 rectangles.")
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
      (time (apply insert (rtree {:max-children 4}) (map (partial ->Rectangle) sample))))
    (dotimes [n 10]
      (println (format "Java RTree Iteration %s" n))
      (time (reduce #(.add %1 nil %2) (RTree/create) (map create-rectangle sample))))))


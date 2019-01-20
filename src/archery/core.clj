(ns archery.core
  (:require [archery.shape :refer :all]
            [archery.zipper :refer :all]
            [archery.visitor :refer :all]
            [archery.util :refer :all]
            [clojure.core.protocols :refer [datafy]]
            [clojure.pprint :refer [pprint]])
  (:import (com.github.davidmoten.rtree RTree)
           (com.github.davidmoten.rtree.geometry Geometries))
  (:gen-class))

(defn -main []
  (println "Starting benchmark: Inserting 1,000 rectangles.")
  (let [random-shapes (fn [] (let [min-x (rand-int 500000)
                                   max-x (+ min-x (rand-int 100000))
                                   min-y (rand-int 500000)
                                   max-y (+ min-y (rand-int 100000))]
                               [[min-x max-x] [min-y max-y]]))
        sample (take 1000 (repeatedly random-shapes))
        create-rectangle (fn [[[min-x max-x] [min-y max-y]]]
                           (Geometries/rectangle (double min-x)
                                                 (double min-y)
                                                 (double max-x)
                                                 (double max-y)))]
    (pprint (datafy (reduce insert (rtree) (take 10 (map ->Rectangle sample)))))
    (dotimes [n 10]
      (println (format "Clojure RTree Iteration %s" n))
      (time
        (reduce insert (rtree {:max-children 5, :min-children 2}) (map ->Rectangle sample))))
    (dotimes [n 10]
      (println (format "Java RTree Iteration %s" n))
      (time (reduce #(.add %1 nil %2) (RTree/create) (map create-rectangle sample))))))

(-main)
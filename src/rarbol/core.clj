(ns rarbol.core
  (:require [rarbol.shape :refer :all]
            [rarbol.zipper :refer :all]
            [rarbol.visitor :refer :all]
            [rarbol.util :refer :all])
  (:gen-class))

(defn insert
  ([rt shape]
   (update rt :tree #(tree-insert % shape)))
  ([rt shape & shapes]
   (reduce insert (insert rt shape) shapes)))

(defn -main []
  (println "Starting benchmark: Inserting 1M rectangles into RTree {:max-children 50}")
  (let [random-rectangle (fn [] (let [min-x (rand-int 500000)
                                      max-x (+ min-x (rand-int 100000))
                                      min-y (rand-int 500000)
                                      max-y (+ min-y (rand-int 100000))]
                                  (->Rectangle [[min-x max-x] [min-y max-y]])))
        sample (take 10000 (repeatedly random-rectangle))]
    (println
      (dotimes [n 10]
        (println (format "Iteration %s" n))
        (time (apply insert (rtree) sample))))))

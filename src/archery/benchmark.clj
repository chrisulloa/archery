(ns archery.benchmark
  (:require [archery.shape :refer [->Rectangle]]
            [archery.core :refer [rtree insert]]
            [clojure.core.protocols :refer [datafy]]
            [clojure.pprint :refer [pprint]]
            [criterium.core :as cm])
  (:import (com.github.davidmoten.rtree RTree)
           (com.github.davidmoten.rtree.geometry Geometries Rectangle))
  (:gen-class))

(defn add-to-java-rtree
  ^RTree
  [^RTree rtree ^Rectangle rectangle]
  (.add rtree nil rectangle))

(defn java-rectangle
  ^Rectangle
  [^Double x1 ^Double y1 ^Double x2 ^Double y2]
  (Geometries/rectangle x1 y1 x2 y2))

(defn java-rtree->string
  ^String
  [^RTree rtree]
  (.asString rtree))

(defn make-java-rtree
  ^RTree []
  (.create (.maxChildren (RTree/minChildren 2) 4)))

(defn random-shapes []
  (let [min-x (rand-int 500000)
        max-x (+ min-x (rand-int 100000))
        min-y (rand-int 500000)
        max-y (+ min-y (rand-int 100000))]
    [(double min-x) (double min-y) (double max-x) (double max-y)]))

(defn bench-against-java-library []
  (let [sample-size 20000
        sample (take sample-size (repeatedly random-shapes))
        smaller-sample (take 20 (repeatedly random-shapes))
        create-rectangle (fn [[x1 y1 x2 y2]] (->Rectangle (double x1)
                                                          (double y1)
                                                          (double x2)
                                                          (double y2)))
        rectangles (into [] (map create-rectangle) sample)
        create-java-rectangle (fn [[x-min y-min x-max y-max]]
                                (java-rectangle x-min y-min x-max y-max))
        java-rectangles (into [] (map create-java-rectangle) sample)]
    (println "Example Clojure RTree")
    (pprint (datafy (reduce insert (rtree) (map create-rectangle smaller-sample))))
    (println "\nExample Java RTree")
    (println
      (java-rtree->string (reduce add-to-java-rtree
                                  (RTree/create)
                                  (map create-java-rectangle smaller-sample))))
    (println (format "Starting benchmark: Inserting %s rectangles." sample-size))
    (println "Java RTree Benchmark")
    (cm/bench (reduce add-to-java-rtree (make-java-rtree) java-rectangles))
    (println "Archery Benchmark:")
    (cm/bench (reduce insert (rtree) rectangles))
    (println "Benchmarking done!")))

(defn -main [] (bench-against-java-library))

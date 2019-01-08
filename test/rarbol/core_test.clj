(ns rarbol.core-test
  (:require [clojure.test :refer :all]
            [rarbol.core :refer :all]
            [rarbol.shape :refer :all]
            [rarbol.util :refer :all]
            [rarbol.visitor :refer :all]))

(deftest test-fast-contains?
  (testing "Fast contains function."
    (is (= true (fast-contains? [10 15 30 5] 30)))))

(deftest test-visitors
  (testing "Visitor functions."
    (let [child1 (map->Rectangle
                   {:shape [[0 50] [0 50]]
                    :leaf?  true
                    :values [(->Point [1 1])
                               (->Point [5 5])
                               (->Rectangle [[10 15] [10 15]])]})
          child2 (map->Rectangle
                   {:shape [[50 100] [50 100]]
                    :leaf?  true
                    :values [(->Point [60 60])
                             (->Rectangle [[55 60] [55 60]])]})
          tree (map->Rectangle
                 {:shape [[50 100] [50 100]]
                  :children [child1 child2]})]
      (is (= #{child1 child2} (set (leaf-collector tree))))
      (is (= child2  (insertion-finder tree (->Rectangle [[65 70] [65 70]]))))
      (is (= child1 (insertion-finder tree (->Point [-5 -5]))))
      (is (= child2 (insertion-finder tree (->Rectangle [[110 115] [110 115]]))))
      (is (= child1 (node-contains-shape-finder tree (->Point [1 1]))))
      (is (= child2 (node-contains-shape-finder tree (->Rectangle [[55 60] [55 60]]))))
      (is (= nil (node-contains-shape-finder tree (->Point [300 300]))))
      (is (= #{(->Point [1 1]) (->Point [5 5]) (->Rectangle [[10 15] [10 15]])}
             (set (enveloped-shapes-collector tree (->Rectangle [[0 20] [0 20]]))))))))

(deftest test-minimum-bounding-rectangle
  (testing "Minimum bounding rectangle."
    (is (= (->Rectangle [[-10 55] [-15 60]])
           (minimum-bounding-rectangle (->Point [0 0])
                                       (->Point [55 60])
                                       (->Point [-10 -15]))))
    (is (= (->Rectangle [[-100 100] [-300 300]])
           (minimum-bounding-rectangle (->Point [-100 -300])
                                       (->Point [100 300])
                                       (->Rectangle [[55 60] [25 100]]))))
    (is (= (->Rectangle [[100 100] [150 150]])
           (minimum-bounding-rectangle (->Rectangle [[100 100] [150 150]]))))))

(deftest test-intersects?
  (testing "Intersects function."
    (is (= true (intersects? (->Rectangle [[100 150] [25 300]])
                             (->Rectangle [[50 300] [40 500]]))))
    (is (= false (intersects? (->Rectangle [[50 60] [50 60]])
                              (->Rectangle [[10 15] [10 15]]))))
    (is (= true (intersects? (->Rectangle [[100 300] [100 300]])
                             (->Rectangle [[150 250] [150 250]]))))))

(deftest test-envelops?
  (testing "Envelops function."
    (is (= true (envelops? (->Rectangle [[100 300] [100 300]])
                           (->Rectangle [[150 250] [150 250]]))))
    (is (= false (envelops? (->Rectangle [[100 150] [25 300]])
                            (->Rectangle [[50 300] [40 500]]))))))

(deftest test-collect-points
  (testing "Collect points function."
    (is (= [[0] [3]] (collect-points (->Point [0 3]))))
    (is (= [[0 10] [5 15]] (collect-points (->Rectangle [[0 10] [5 15]]))))))
(ns archery.core-test
  (:require [clojure.test :refer :all]
            [archery.core :refer :all]
            [archery.shape :refer :all]
            [archery.util :refer :all]
            [archery.visitor :refer :all]))

(deftest test-fast-contains?
  (testing "Fast contains function."
    (is (= true (fast-contains? [10 15 30 5] 30)))))

(deftest test-visitors
  (testing "Visitor functions."
    (let [child1 (map->Rectangle
                   {:shape    [[0 50] [0 50]]
                    :leaf?    true
                    :children [(->Point [1 1])
                               (->Point [5 5])
                               (map->Rectangle {:shape [[10 15] [10 15]]})]})
          child2 (map->Rectangle
                   {:shape    [[50 100] [50 100]]
                    :leaf?    true
                    :children [(->Point [60 60])
                               (simple-rectangle [[55 60] [55 60]])]})
          tree (map->Rectangle
                 {:shape    [[0 100] [0 100]]
                  :children [child1 child2]})]
      (is (= #{child1 child2} (set (leaf-collector tree))))
      (is (= child1 (node-contains-shape-finder tree (->Point [1 1]))))
      (is (= child2 (node-contains-shape-finder tree (simple-rectangle [[55 60] [55 60]]))))
      (is (= nil (node-contains-shape-finder tree (->Point [300 300]))))
      (is (= #{(->Point [1 1]) (->Point [5 5]) (simple-rectangle [[10 15] [10 15]])}
             (set (enveloped-shapes-collector tree (simple-rectangle [[0 20] [0 20]]))))))))

(deftest test-minimum-bounding-rectangle
  (testing "Minimum bounding rectangle."
    (is (= (map->Rectangle {:shape [[-10 55] [-15 60]] :leaf? true})
           (minimum-bounding-rectangle (->Point [0 0])
                                       (->Point [55 60])
                                       (->Point [-10 -15]))))
    (is (= (map->Rectangle {:leaf? true, :shape [[-100 100] [-300 300]]})
           (minimum-bounding-rectangle (->Point [-100 -300])
                                       (->Point [100 300])
                                       (simple-rectangle [[55 60] [25 100]]))))
    (is (= (map->Rectangle {:leaf? true, :shape [[100 100] [150 150]]})
           (minimum-bounding-rectangle (simple-rectangle [[100 100] [150 150]]))))))

(deftest test-intersects?
  (testing "Intersects function."
    (is (= true (intersects? (simple-rectangle [[100 150] [25 300]])
                             (simple-rectangle [[50 300] [40 500]]))))
    (is (= false (intersects? (simple-rectangle [[50 60] [50 60]])
                              (simple-rectangle [[10 15] [10 15]]))))
    (is (= true (intersects? (simple-rectangle [[100 300] [100 300]])
                             (simple-rectangle [[150 250] [150 250]]))))))

(deftest test-envelops?
  (testing "Envelops function."
    (is (= true (envelops? (map->Point {:shape [5 10] :foo ""})
                           (->Point [5 10]))))
    (is (= false (envelops? (->Point [5 10]) (->Point [0 0]))))
    (is (= false (envelops? (->Point [5 10]) (simple-rectangle [[5 10] [35 35]]))))
    (is (= true (envelops? (simple-rectangle [[5 10] [35 39]]) (->Point [6 38]))))
    (is (= true (envelops? (simple-rectangle [[100 300] [100 300]])
                           (simple-rectangle [[150 250] [150 250]]))))
    (is (= false (envelops? (simple-rectangle [[100 150] [25 300]])
                            (simple-rectangle [[50 300] [40 500]]))))))

(deftest test-collect-points
  (testing "Collect points function."
    (is (= [[0] [3]] (collect-points (->Point [0 3]))))
    (is (= [[0 10] [5 15]] (collect-points (simple-rectangle [[0 10] [5 15]]))))))

(deftest test-shape->rectangle
  (testing "shape->rectangle"
    (is (= (simple-rectangle [[5 10] [5 10]])
           (shape->rectangle (simple-rectangle [[5 10] [5 10]]))))
    (is (= (simple-rectangle [[5 5] [10 10]])
           (shape->rectangle (->Point [5 10]))))))

(deftest test-augment-shape
  (testing "augment-shape"
    (is (= {0 [10 10], 1 [15 15]}
           (:augmented (augment-shape (->Point [10 15])))))
    (is (= {0 [10 15], 1 [35 40]}
           (:augmented (augment-shape (simple-rectangle [[10 15] [35 40]])))))))

(deftest test-augmented-val
  (is (= 10 ((augmented-val second 2)
              (augment-shape (simple-rectangle [[10 15] [3 5] [0 10]]))))))

(deftest test-area-enlargement-diff
  (is (= 25 (area-enlargement-diff (simple-rectangle [[0 5] [0 5]]) (->Point [5 10])))))

(deftest test-compress-rectangle
  (is (= (simple-rectangle [[5 10] [5 10]])
         (compress-rectangle (simple-rectangle [[5 10] [5 10]]))))
  (is (= (:shape (simple-rectangle [[5 5] [5 5]]))
         (:shape (compress-rectangle (simple-rectangle [[0 0] [0 0]])
                                     (->Point [5 5])))))
  (is (= (:shape (simple-rectangle [[5 10] [5 10]]))
         (:shape (compress-rectangle (simple-rectangle [[5 8] [5 8]])
                                     (->Point [5 5])
                                     (->Point [5 10])
                                     (->Point [10 10]))))))

(deftest test-linear-seeds
  (let [shapes [(->Point [0 0])
                (->Point [10 10])
                (map->Rectangle {:shape [[0 3] [6 8]]})
                (map->Rectangle {:shape [[15 30] [35 55]]})]
        seeds (linear-seeds-across-dimensions shapes)]
    (is (= {:dimension       0
            :seeds           [(->Point [0 0]) (map->Rectangle {:shape [[15 30] [35 55]]})]
            :norm-separation (/ 15 30)}
          (first seeds)))
    (is (= {:dimension       1
            :seeds           [(->Point [0 0]) (map->Rectangle {:shape [[15 30] [35 55]]})]
            :norm-separation (/ 35 55)}
           (second seeds)))
    (is (= [(->Point [0 0]) (map->Rectangle {:shape [[15 30] [35 55]]})]
           (linear-seeds shapes)))))

(deftest test-initialize-seed
  (let [point-seed (->Point [15 30])
        rectangle-seed (simple-rectangle [[15 30] [35 45]])]
    (is (= (map->Rectangle {:shape [[15 15] [30 30]]
                            :leaf? true
                            :children [point-seed]})
           (initialize-seed point-seed true)))
    (is (= (map->Rectangle {:shape [[15 30] [35 45]]
                            :leaf? true
                            :children [rectangle-seed]})
           (initialize-seed rectangle-seed true)))))

(deftest test-shape->seed
  (let [l-seed (map->Rectangle {:leaf? true,
                                :children [(->Point [15 30])]
                                :shape [[15 15] [30 30]]})
        r-seed (map->Rectangle {:leaf? true,
                                :children [(simple-rectangle [[35 55] [80 85]])]
                                :shape [[35 55] [80 85]]})
        shape (->Point [18 18])]
    (is (= {:next-seed     :l-seed
            :enlarged-seed (map->Rectangle {:leaf? true,
                                            :shape    [[15 18] [18 30]]
                                            :children [(->Point [15 30]) shape]})}
           (shape->seed shape r-seed l-seed)))))

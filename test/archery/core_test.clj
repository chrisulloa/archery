(ns archery.core-test
  (:require [clojure.test :refer :all]
            [archery.core :refer :all]
            [archery.shape :refer :all]
            [archery.quadratic-node-split :refer :all]
            [archery.linear-node-split :refer :all]
            [archery.util :refer :all]
            [archery.visitor :refer :all]))

(deftest test-fast-contains?
  (testing "Fast contains function."
    (is (= true (fast-contains? [10 15 30 5] 30)))))

(deftest test-visitors
  (testing "Visitor functions."
    (let [child1 (->RectangleNode true
                                  [(->Point 1 1) (->Point 5 5)
                                   (->Rectangle 10 10 15 15)]
                                  0 0 50 50)
          child2 (->RectangleNode true
                                  [(->Point 60 60)
                                   (->Rectangle 55 55 60 60)]
                                  0 0 60 60)
          root (->RectangleNode false [child1 child2] 0 0 50 50)
          tree (->RTree root 2 4 quadratic-split)]
      (is (= #{child1 child2} (set (leaf-collector root))))
      (is (= child1 (node-contains-shape-finder root (->Point 1 1))))
      (is (= child2 (node-contains-shape-finder root (->Rectangle 55 55 60 60))))
      (is (= nil (node-contains-shape-finder root (->Point 300 300))))
      (is (= #{(->Point 1 1) (->Point 5 5) (->Rectangle 10 10 15 15)}
             (set (search tree (->Rectangle 0 0 20 20))))))))

(deftest test-minimum-bounding-rectangle
  (testing "Minimum bounding rectangle."
    (is (= (->Rectangle -10.0 -15.0 55.0 60.0)
           (reduce minimum-bounding-rectangle [(->Point 0 0)
                                               (->Point 55 60)
                                               (->Point -10 -15)])))
    (is (= (->Rectangle -100.0 -300.0 100.0 300.0)
           (reduce minimum-bounding-rectangle
                   [(->Point -100.0 -300.0)
                    (->Point 100.0 300.0)
                    (->Rectangle 55.0 25.0 60.0 100.0)])))
    (is (= (->Rectangle 100 150 100 150)
           (minimum-bounding-rectangle (->Rectangle 100 150 100 150))))))

(deftest test-intersects?
  (testing "Intersects function."
    (is (= true (intersects? (->Rectangle 100 25 150 300)
                             (->Rectangle 50 40 300 500))))
    (is (= false (intersects? (->Rectangle 50 50 60 60)
                              (->Rectangle 10 10 15 15))))
    (is (= true (intersects? (->Rectangle 100 100 300 300)
                             (->Rectangle 150 150 250 250))))))

(deftest test-envelops?
  (testing "Envelops function."
    (is (= true (envelops? (map->Point {:x 5 :y 10 :foo ""})
                           (->Point 5 10))))
    (is (= false (envelops? (->Point 5 10) (->Point 0 0))))
    (is (= false (envelops? (->Point 5 10) (->Rectangle 5 35 10 35))))
    (is (= true (envelops? (->Rectangle 5 35 10 39) (->Point 6 38))))
    (is (= true (envelops? (->Rectangle 100 100 300 300)
                           (->Rectangle 150 150 250 250))))
    (is (= false (envelops? (->Rectangle 100 25 150 300)
                            (->Rectangle 50 40 300 500))))))

(deftest test-rectangle-shape
  (testing "rectangle-shape"
    (is (= [0 3 0 3] (rectangle-shape (->Point 0 3))))
    (is (= [0 5 10 15] (rectangle-shape (->Rectangle 0 5 10 15))))))

(deftest test-area-enlargement-diff
  (is (= 25.0 (area-enlargement (->RectangleNode true [] 0 0 5 5) (->Point 5 10)))))

(deftest test-compress-rectangle
  (is (= [5 5 10 10]
         (shape (compress (->RectangleNode true [] 5 5 10 10)))))
  (is (= [5.0 5.0 5.0 5.0]
         (shape (compress (->RectangleNode true [(->Point 5.0 5.0)] 0 0 0 0)))))
  (is (= [5.0 5.0 10.0 10.0]
         (shape (compress (->RectangleNode
                            true [(->Point 5 5) (->Point 5 10) (->Point 10 10)]
                            5 5 8 8))))))

(deftest test-linear-seeds
  (let [shapes [(->Point 0 0)
                (->Point 10 10)
                (->Rectangle 0 6 3 8)
                (->Rectangle 15 35 30 55)]]
    (is (= (set [[15 35 30 55] [0 0 0 0]])
           (set (map shape (linear-seeds shapes true)))))))

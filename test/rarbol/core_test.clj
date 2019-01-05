(ns rarbol.core-test
  (:require [clojure.test :refer :all]
            [rarbol.core :refer :all]
            [rarbol.shape :refer :all]
            [rarbol.util :refer :all]
            [rarbol.visitor :refer :all]))

(deftest fast-contains
  (testing "Fast contains function."
    (is (= true (fast-contains? [10 15 30 5] 30)))))

(deftest visitors
  (testing "Visitor functions."
    (let [child1 (map->Rectangle
                   {:x0 0 :x1 50
                    :y0 0 :y1 50
                    :leaf? true
                    :shapes [(->Point 1 1)
                             (->Point 5 5)
                             (->Rectangle 10 15 10 15)]})
          child2 (map->Rectangle
                   {:x0     50 :x1 100
                    :y0     50 :y1 100
                    :leaf?  true
                    :shapes [(->Point 60 60)
                             (->Rectangle 55 60 55 60)]})
          tree (map->Rectangle
                 {:x0       0 :x1 100
                  :y0       0 :y1 100
                  :children [child1 child2]})]
      (is (= #{child1 child2} (leaf-collector tree)))
      (is (= child2  (insertion-finder tree (->Rectangle 65 70 65 70))))
      (is (= child1 (insertion-finder tree (->Point -5 -5))))
      (is (= child2 (insertion-finder tree (->Rectangle 110 115 110 115))))
      (is (= child1 (node-contains-shape-finder tree (->Point 1 1))))
      (is (= child2 (node-contains-shape-finder tree (->Rectangle 55 60 55 60))))
      (is (= nil (node-contains-shape-finder tree (->Point 300 300))))
      (is (= #{(->Point 1 1) (->Point 5 5) (->Rectangle 10 15 10 15)}
             (set (enveloped-shapes-collector tree (->Rectangle 0 20 0 20))))))))
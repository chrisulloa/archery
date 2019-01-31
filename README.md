# archery

Clojure RTree implementation, using functional zippers. Very much a work in progress.

<img src="https://raw.githubusercontent.com/chrisulloa/archery/master/doc/visualization%20(2).png" width="550">

## Examples
To create and load an RTree with shapes:
```
(use '[rtree.core])

; Default values, can also ues {:node-split :linear}
(def empty-tree (rtree {:node-split :quadratic,
                        :min-children 2,
                        :max-chidlren 4}))

(def tree
  (reduce insert empty-tree [(->Point 0.5 10.5)
                             (->Point 33.3 45.0)
                             (->Rectangle 0.0 0.0 10.0 10.0)
                             (->Rectangle 5.0 15.0 30.0 55.0)
                             (->Point 3.0 10.0)]))
```

You can also search by a shape that has an envelops? function:
```
(search tree (->Rectangle 0.0 0.0 50.0 50.0))
;=>
(#archery.shape.Point{:x 33.3, :y 45.0}
 #archery.shape.Rectangle{:x1 0.0, :y1 0.0, :x2 10.0, :y2 10.0}
 #archery.shape.Point{:x 0.5, :y 10.5}
 #archery.shape.Point{:x 3.0, :y 10.0})
```

Some convenience functions to view and visualize the RTree:
```
(datafy tree)
;=>
{:type :RTree,
 :max-children 4,
 :min-children 2,
 :root {:type :RectangleNode,
        :leaf? false,
        :shape [0.0 0.0 33.3 55.0],
        :children [{:type :RectangleNode,
                    :leaf? true,
                    :shape [5.0 15.0 33.3 55.0],
                    :children [{:type :Point, :shape [33.3 45.0]}
                               {:type :Rectangle, :shape [5.0 15.0 30.0 55.0]}]}
                   {:type :RectangleNode,
                    :leaf? true,
                    :shape [0.0 0.0 10.0 10.5],
                    :children [{:type :Rectangle, :shape [0.0 0.0 10.0 10.0]}
                               {:type :Point, :shape [0.5 10.5]}
                               {:type :Point, :shape [3.0 10.0]}]}]}}
                               

 
(use '[archery.visualization])
(tree->vega-lite tree)
;=>
{:background "white",
 :width 1000,
 :height 1000,
 :layer [{:data {:values [{:x1 0.0, :y1 0.0, :x2 33.3, :y2 55.0}
                          {:x1 5.0, :y1 15.0, :x2 33.3, :y2 55.0}
                          {:x1 0.0, :y1 0.0, :x2 10.0, :y2 10.5}]},
          :mark "rect",
          :encoding {:x {:field "x1", :type "quantitative"},
                     :x2 {:field "x2"},
                     :y {:field "y1", :type "quantitative"},
                     :y2 {:field "y2"},
                     :opacity {:value 1},
                     :stroke {:value "green"}}}
         {:data {:values [{:x1 33.3, :y1 45.0, :x2 33.3, :y2 45.0}
                          {:x1 0.5, :y1 10.5, :x2 0.5, :y2 10.5}
                          {:x1 3.0, :y1 10.0, :x2 3.0, :y2 10.0}]},
          :mark "circle",
          :encoding {:color {:value "red"},
                     :x {:field "x1", :type "quantitative"},
                     :x2 {:field "x2"},
                     :y {:field "y1", :type "quantitative"},
                     :y2 {:field "y2"}}}
         {:data {:values [{:x1 5.0, :y1 15.0, :x2 30.0, :y2 55.0}
                          {:x1 0.0, :y1 0.0, :x2 10.0, :y2 10.0}]},
          :mark "rect",
          :encoding {:x {:field "x1", :type "quantitative"},
                     :x2 {:field "x2"},
                     :y {:field "y1", :type "quantitative"},
                     :y2 {:field "y2"},
                     :stroke {:value "blue"},
                     :opacity {:value 0.8}}}]}
```

## Features

* Algorithms
  * Searching
    - [ ] RadiusSearch
    - [ ] ShapeIntersects
    - [x] RectangleContains
    - [ ] Composable Query API
  * Insertion
    - [x] ChooseLeaf
    - [x] AdjustTree
    - [ ] BulkLoading
  * Deletion
    - [ ] FindLeaf
    - [ ] CondenseTree
  * Updating
  * Splitting
    - [x] QuadraticSplit
    - [x] LinearSplit
    - [ ] R\*Tree NodeSplit
* Future Plans
   - [ ] Hilbert RTree
   - [ ] Geohash
   - [ ] Z-Order Space Filling Curve
* Visualizations
  - [x] Vega JSON Output
* Benchmarks
  - [ ] Criterium

## References
Alex Miller: Tree visitors in Clojure

https://www.ibm.com/developerworks/library/j-treevisit/

A. Guttman. R-trees: A Dynamic Index Structure for Spatial Searching. Proceedings of ACM SIGMOD, pages 47-57, 1984.

http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf

## License

Distributed under the Eclipse Public License version 1.0.

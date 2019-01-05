# rarbol

rtree implementation in Clojure, using functional zippers.

## Example Usage

```
(def tree (map->Rectangle
            {:x0 0 :x1 100
             :y0 0 :y1 100
             :children [(map->Rectangle
                          {:x0 0 :x1 50
                           :y0 0 :y1 50
                           :leaf? true
                           :shapes [(->Point 1 1)
                                    (->Point 5 5)]})
                        (map->Rectangle
                          {:x0 50 :x1 100
                           :y0 50 :y1 100
                           :leaf? true
                           :shapes [(->Point 60 60)]})]}))
=> #'rarbol.core/tree       

(enveloped-shapes-collector tree (->Rectangle 0 10 0 10))
=> (#rarbol.shape.Point{:x 1, :y 1} #rarbol.shape.Point{:x 5, :y 5})

(enveloped-shape-finder tree (->Point 60 60))
=>
#rarbol.shape.Rectangle{:x0 50,
                        :x1 100,
                        :y0 50,
                        :y1 100,
                        :leaf? true,
                        :shapes [#rarbol.shape.Point{:x 60, :y 60}]}
                          
(area (shape-finder (->Point 60 60)))
=> 2500

(enveloped-shapes-collector tree (->Rectangle 0 60 0 60))
=> (#rarbol.shape.Point{:x 1, :y 1} #rarbol.shape.Point{:x 5, :y 5} #rarbol.shape.Point{:x 60, :y 60})

(enveloped-shapes-collector tree (->Rectangle 0 55 0 55))
=> (#rarbol.shape.Point{:x 1, :y 1} #rarbol.shape.Point{:x 5, :y 5})

(insertion-finder tree (->Point 8 8))
=>
#rarbol.shape.Rectangle{:x0 0,
                        :x1 50,
                        :y0 0,
                        :y1 50,
                        :leaf? true,
                        :shapes [#rarbol.shape.Point{:x 1, :y 1} #rarbol.shape.Point{:x 5, :y 5}]}
```

## TODO
* Searching
  * RadiusSearch
  * ShapeIntersects
  * ShapeContains
* Insertion
  * ChooseLeaf
  * AdjustTree
  * NodeSplitting
* Deletion
  * FindLeaf
  * CondenseTree
* Updating
* Splitting
  * QuadraticSplit
  * LinearSplit
* R\*Tree
* N-Dimensional Shapes
* Benchmarking

## References

https://www.ibm.com/developerworks/library/j-treevisit/

http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf

## License

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

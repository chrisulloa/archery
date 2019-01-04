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
                           :children [(->Point 1 1)
                                      (->Point 5 5)]})
                        (map->Rectangle
                          {:x0 50 :x1 100
                           :y0 50 :y1 100
                           :leaf? true
                           :children [(->Point 60 60)]})]}))
=> #'rarbol.core/tree
                         
(shape-finder tree (->Point 60 60))
=>
#{#rarbol.shape.Rectangle{:x0 50,
                          :x1 100,
                          :y0 50,
                          :y1 100,
                          :leaf? true,
                          :children [#rarbol.shape.Point{:x 60, :y 60}]}}
```

## TODO
* Insertion
* Deletion
* Condensation
* Node Splitting

## References

https://www.ibm.com/developerworks/library/j-treevisit/

http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf

## License

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

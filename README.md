# archery

Clojure RTree implementation, using functional zippers. Very much a work in progress.

<img src="https://raw.githubusercontent.com/chrisulloa/archery/master/doc/visualization%20(2).png" width="550">

## Examples

## Features

* Algorithms
  * Searching
    - [ ] RadiusSearch
    - [ ] ShapeIntersects
    - [x] RectangleContains
    - [x] AllShapes
  * Insertion
    - [ ] ChooseLeaf
    - [x] AdjustTree
    - [ ] BulkLoading
  * Deletion
    - [ ] FindLeaf
    - [ ] CondenseTree
  * Updating
  * Splitting
    - [ ] QuadraticSplit
    - [x] LinearSplit
    - [ ] R\*TreeNodeSplit
* Future Plans
   - [ ] Hilbert RTree
   - [ ] Geohash
   - [ ] Z-Order Space Filling Curve
* Visualizations
  - [ ] Vega JSON Output
* Benchmarks
  - [ ] Criterium

## References
Alex Miller: Tree visitors in Clojure

https://www.ibm.com/developerworks/library/j-treevisit/

A. Guttman. R-trees: A Dynamic Index Structure for Spatial Searching. Proceedings of ACM SIGMOD, pages 47-57, 1984.

http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf

## License

Distributed under the Eclipse Public License version 1.0.

# archery

Clojure RTree implementation, using functional zippers. Very much a work in progress.

![alt text](https://raw.githubusercontent.com/chrisulloa/archery/master/doc/visualization%20(2).png)

## Features
* Visualizations
  * Vega JSON Output
* Algorithms
  * Searching
    * RadiusSearch
    * ShapeIntersects
    * ShapeContains
  * Insertion
    * ChooseLeaf
    * AdjustTree
    * NodeSplitting
    * BulkLoading
  * Deletion
    * FindLeaf
    * CondenseTree
  * Updating
  * Splitting
    * QuadraticSplit
    * LinearSplit
    * R\*TreeNodeSplit
* N-Dimensions
  * Hilbert RTree
* Benchmarks

## References

https://www.ibm.com/developerworks/library/j-treevisit/

https://github.com/davidmoten/rtree

http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf

## License

Distributed under the Eclipse Public License version 1.0.

# archery

rtree implementation in Clojure, using functional zippers.

## TODO
Visualizations
* Vega
Algorithms
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
  * R\*TreeNodeSplit
N-Dimensions
  * Hilbert RTree
Benchmarks
* Benchmark against David Moten library

## References

https://www.ibm.com/developerworks/library/j-treevisit/

https://github.com/davidmoten/rtree

https://www.researchgate.net/publication/260599600_Corner-based_splitting_An_improved_node_splitting_algorithm_for_R-tree

http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf

## License

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

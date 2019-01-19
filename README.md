# archery

rtree implementation in Clojure, using functional zippers.

## TODO
* At the lowest level store records, for the RTree itself use datatypes (should improve performance significantly).
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

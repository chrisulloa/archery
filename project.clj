(defproject archery "0.1.0-SNAPSHOT"
  :description "Clojure RTree implementation, using functional zippers."
  :url "https://github.com/chrisulloa/archery"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot archery.benchmark
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.github.davidmoten/rtree "0.8.6"]])

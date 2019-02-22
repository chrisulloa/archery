(defproject archery "0.1.0-SNAPSHOT"
  :description "Clojure RTree implementation, using functional zippers."
  :url "https://github.com/chrisulloa/archery"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :target-path "target/%s"
  :source ["src/archery/clj"]
  :java-source-paths ["src/archery/util"]
  :test-paths ["test" "test/archery"]
  :main ^:skip-aot archery.clj.benchmark
  :profiles {:uberjar {:aot :all}}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/data.json "0.2.6"]
                 [criterium "0.4.4"]
                 [com.github.davidmoten/rtree "0.8.6"]])

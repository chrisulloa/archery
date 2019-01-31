(defproject archery "0.1.0-SNAPSHOT"
  :description "Clojure RTree implementation, using functional zippers."
  :url "https://github.com/chrisulloa/archery"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[io.taylorwood/lein-native-image "0.3.0"]]
  :native-image {:graal-bin "/Users/Christian/graalvm-ce-1.0.0-rc11/Contents/Home/bin"
                 :opts ["--verbose"]
                 :name "archery"}
  :main archery.benchmark
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :native-image {:opts ["-Dclojure.compiler.direct-linking=true"]
                                      :graal-bin "/Users/Christian/graalvm-ce-1.0.0-rc11/Contents/Home/bin"}}}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [com.github.davidmoten/rtree "0.8.6"]])

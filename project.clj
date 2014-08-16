(defproject civs "0.2.0-SNAPSHOT"
  :description "A simulator of civilizations evolution"
  :url "https://github.com/ftomassetti/civs"
  :license {:name "Apache License v 2.0"
            :url "http://www.apache.org/licenses/"}
  :dependencies [
                  [org.clojure/clojure "1.6.0"]
                  [com.github.lands/lands-java-lib "0.3-20140813.200955-2"]
                  [org.clojure/math.combinatorics "0.0.8"]
                  [org.clojure/tools.cli "0.3.1"]
                  [org.clojure/data.json "0.2.5"]]
  :scm git
  :main ^:skip-aot civs.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :repositories {"sonartype snapshots" "https://oss.sonatype.org/content/repositories/snapshots"})

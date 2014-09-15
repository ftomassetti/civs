(defproject civs "0.2.4-SNAPSHOT"
  :description "A simulator of civilizations evolution"
  :url "https://github.com/ftomassetti/civs"
  :license {:name "Apache License v 2.0"
            :url "http://www.apache.org/licenses/"}
  :dependencies [
                  [org.clojure/clojure "1.6.0"]
                  [com.github.lands/lands-java-lib "0.3-SNAPSHOT"]
                  [org.clojure/math.combinatorics "0.0.8"]
                  [org.clojure/tools.cli "0.3.1"]
                  [com.velisco/tagged "0.3.0"]
                  [org.clojure/data.fressian "0.2.0"] ]
  :scm git
  :main ^:skip-aot civs.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :repositories {"sonartype snapshots" "https://oss.sonatype.org/content/repositories/snapshots"}
  :test-selectors {:default (complement :acceptance)
                   :acceptance :acceptance
                   :all (constantly true)})

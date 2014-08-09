(defproject civs "0.1.0-SNAPSHOT"
  :description "A simulator of civilizations evolution"
  :url "https://github.com/ftomassetti/civs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                  [org.clojure/clojure "1.6.0"]
                  [com.github.lands/lands-java-lib "0.1-SNAPSHOT"]]
  :main ^:skip-aot civs.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

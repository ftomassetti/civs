(ns
  ^{:author ftomassetti}
  civs.cli
  (:require [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  ;; An option with a required argument
  [
    ["-w" "--world WORLD_FILE" "World file to be used"]
    ["-h" "--help"]
  ])

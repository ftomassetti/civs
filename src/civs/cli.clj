(ns
  ^{:author ftomassetti}
  civs.cli
  (:require [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  ;; An option with a required argument
  [
    ["-w" "--world WORLD_FILE" "World file to be used"]
    ["-h" "--help"]
    ["-b" "--initial-bands NBANDS" "Number of initial bands populatin the world"
    :default 100
    :parse-fn #(Integer/parseInt %)
    :validate [#(and (>= % 1) (<= % 1000)) "Must be a number in [1,1000]"]]
    ["-t" "--turns NTURNS" "Number of turns to be simulated"
     :default 100
     :parse-fn #(Integer/parseInt %)
     :validate [#(and (>= % 1) (<= % 1000)) "Must be a number in [1,1000]"]]
    ["-f" "--history-filename HISTORY_FILENAME" "Name of the history file to be generated"
     :default "my.history"]
    ["-r" "--readable-format" "Produce an history filename is human readbale format"]
  ])

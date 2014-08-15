(ns civs.core
  (:gen-class)
  (:require
    [civs.cli :refer :all]
    [civs.model :refer :all]
    [civs.logic :refer :all]
    [civs.logic.basic :refer :all]
    [civs.logic.demographics :refer :all]
    [civs.logic.tribe-choices :refer :all]
    [civs.logic.stats :refer :all]
    [civs.society :refer :all]
    [civs.graphics :refer :all]
    [clojure.tools.cli :refer [parse-opts]]
    [clojure.string :as string]
    [clojure.data.json :as json]))

(import '(com.github.lands.IncorrectFileException))
(import '(java.io.IOException))

(defn failure [msg]
  (binding [*out* *err*]
    (println "Error:" msg)
    (println "")
    (println "Use -h for help")
    (println "Exit."))
  (System/exit 1))

(defn usage [options-summary]
  (println (->> ["This program run simulations of civilizations evolution and struggling"
        ""
        "Usage: [lein run] civs [options]"
        ""
        "Options:"
        options-summary
        ""
        "Feel free to ask all possible questions on https://github.com/ftomassetti/civs (just open an issue!)"]
    (string/join \newline)))
    (System/exit 0))

(defn simulate
  "Return a map of history and game-snapshots"
  [initial-game n-turns]
  (def current-game initial-game)
  (let [game-snapshots (atom {0 initial-game})]
    (dotimes [t n-turns]
      (do
        (println "=== Turn" (inc t) "===")
        (def current-game (turn current-game))
        (swap! game-snapshots assoc (int t) current-game)
        (println "  population  " (game-total-pop current-game))
        (println "  bands       " (n-bands-alive current-game))
        (println "  tribes      " (n-tribes-alive current-game))
        (println "  chiefdoms   " (n-chiefdoms-alive current-game))
        (println "  settlements " (.size (settlements current-game)))
        (println "")))
      {:facts (deref facts), :game-snapshots (deref game-snapshots)}))

(defn my-json-converter [world-filename]
  (fn [key value]
    (cond
      (instance? com.github.lands.World value) world-filename
      :default value)))

(defn- save-simulation-result [simulation-result history-filename world-filename]
  (let [json-str (json/write-str simulation-result :value-fn (my-json-converter world-filename))]
    (spit history-filename json-str)))

(defn run [world-filename n-bands n-turns history-filename]
  (println "World            :" world-filename)
  (println "Initial bands    :" n-bands)
  (println "No. turns        :" n-turns)
  (println "History filename :" history-filename)
  (println "")
  (try
    (let [w (load-world "examples-worlds/seed_77.world")
          g (generate-game w n-bands)
          simulation-result (simulate g n-turns)]
      (save-simulation-result simulation-result history-filename world-filename))
    (catch java.io.IOException e (failure "The world cannot be loaded because of an IO error"))
    (catch com.github.lands.IncorrectFileException e (failure "The world cannot be loaded because it contains errors"))))

(defn -main [& args]
  (println " Civs : a civilizations simulator ")
  (println "----------------------------------")
  (println "")
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (nil? (:world options))
        (failure "World to be used not specified (option -w missing)")
      (:help options)
        (usage summary)
      errors (failure errors))
    (run (:world options) (:initial-bands options) (:turns options) (:history-filename options))))

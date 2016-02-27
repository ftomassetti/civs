(ns civs.core
  (:gen-class)
  (:require
    [civs.cli :refer :all]
    [civs.io :refer :all]
    [civs.model.core :refer :all]
    [civs.model.society :refer :all]
    [civs.logic.core :refer :all]
    [civs.logic.globals :refer :all]
    [civs.logic.basic :refer :all]
    [civs.logic.demographics :refer :all]
    [civs.logic.tribe-choices :refer :all]
    [civs.logic.stats :refer :all]
    [civs.graphics :refer :all]
    [clojure.tools.cli :refer [parse-opts]]
    [clojure.string :as string]))

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
  (println
    (string/join
      \newline 
      ["This program run simulations of civilizations evolution and struggling"
      ""
      "Usage: [lein run] civs [options]"
      ""
      "Options:"
      options-summary
      ""
      "Feel free to ask all possible questions on https://github.com/ftomassetti/civs (just open an issue!)"]))
    (System/exit 0))

(defn simulate
  "Return a map of history and game-snapshots"
  [initial-game n-turns & [verbosity]]
  (def current-game initial-game)
  (reset-current-turn)
  (let [game-snapshots (atom {0 initial-game})
        facts-by-turn  (atom {})
        verbosity (if (nil? verbosity) true verbosity)]
    (dotimes [t n-turns]
      (do
        (when verbosity
          (println "=== Turn" (inc t) "==="))
        (inc-current-turn)
        (def current-game (turn current-game))
        (swap! game-snapshots assoc (inc t) current-game)
        (swap! facts-by-turn assoc (inc t) (deref facts))
        (swap! facts (fn [_] []))
        (when verbosity
          (println "  population  " (game-total-pop current-game))
          (println "  bands       " (n-bands-alive current-game))
          (println "  tribes      " (n-tribes-alive current-game))
          (println "  chiefdoms   " (n-chiefdoms-alive current-game))
          (println "  settlements " (.size (populated-settlements current-game)))
          (println ""))))
      {:facts (deref facts-by-turn), :game-snapshots (deref game-snapshots)}))

(defn run [world-filename n-bands n-turns history-filename use-fressian]
  (println "World            :" world-filename)
  (println "Initial bands    :" n-bands)
  (println "No. turns        :" n-turns)
  (println "History filename :" history-filename)
  (println "")
  (try
    (let [w (load-world world-filename)
          g (generate-game w n-bands)
          simulation-result (simulate g n-turns)]
      (save-simulation-result simulation-result history-filename world-filename use-fressian))
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
    (run (:world options) (:initial-bands options) (:turns options) (:history-filename options) (nil? (:readable-format options)))))

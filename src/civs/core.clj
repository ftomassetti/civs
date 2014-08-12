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
    [clojure.string :as string]))

;(require '[civs.model :refer :all])
;(require '[civs.logic :refer :all])

(def w (load-world "examples-worlds/seed_77.world"))

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
      (println "Not yet implemented...")))

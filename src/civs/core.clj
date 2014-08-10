(ns civs.core
  (:gen-class)
  (:require
    [civs.model :refer :all]
    [civs.logic :refer :all]
    [civs.logic.demographics :refer :all]
    [civs.logic.tribe-choices :refer :all]
    [civs.logic.stats :refer :all]))

;(require '[civs.model :refer :all])
;(require '[civs.logic :refer :all])

(def w (load-world "examples-worlds/seed_77.world"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def filename)
  (println "Hello, World!"))

(ns civs.core
  (:gen-class)
  (:require
    [civs.model :refer :all]
    [civs.logic :refer :all]
    [civs.logic.demographics :refer :all]))

;(require '[civs.model :refer :all])
;(require '[civs.logic :refer :all])

(def filename "examples-worlds/seed_77.world")

(def w
  (let [f (java.io.File. filename)]
    (. com.github.lands.PickleSerialization loadWorld f)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def filename)
  (println "Hello, World!"))

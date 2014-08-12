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
    [clojure.tools.cli :refer [parse-opts]]))

;(require '[civs.model :refer :all])
;(require '[civs.logic :refer :all])

(def w (load-world "examples-worlds/seed_77.world"))

(defn -main [& args]
  (parse-opts args cli-options))

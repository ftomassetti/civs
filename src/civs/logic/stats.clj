(ns
  ^{:author ftomassetti}
  civs.logic.stats
  (:require
    [civs.model.core :refer :all]
    [civs.logic.core :refer :all]
    [civs.logic.demographics :refer :all]
    [civs.logic.tribe-choices :refer :all]))

(defn- execute-turns [world tribe nturns]
  (if (and (alive? tribe) (pos? nturns))
    (execute-turns world (group-turn world tribe) (dec nturns))
    tribe))

(defn observe-tribe [world n]
  (let [t (generate-tribe world)
        t (execute-turns world t n)]
    (println "Survivers " (-> t .population total-persons))
    t))

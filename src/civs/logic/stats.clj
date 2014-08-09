(ns
  ^{:author ftomassetti}
  civs.logic.stats
  (:require
    [civs.model :refer :all]
    [civs.logic :refer :all]
    [civs.logic.tribe-choices :refer :all]))

(defn- execute-turns [world tribe nturns]
  (if (and (alive? tribe) (> nturns 0))
    (execute-turns world (turn world tribe) (- nturns 1))
    tribe))

(defn observe-tribe [world n]
  (let [t (generate-tribe world)
        t (execute-turns world t n)]
    (println "Survivers " (-> t .population total-persons))
    t))

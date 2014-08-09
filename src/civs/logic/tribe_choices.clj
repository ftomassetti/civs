(ns
  ^{:author ftomassetti}
  civs.logic.tribe-choices
  (:require
    [civs.model :refer :all]
    [civs.logic :refer :all])
  (:import [civs.model Population Tribe]))

(defn chance-to-become-semi-sedentary [tribe prosperity]
  (if (and (nomadic? tribe) (> prosperity 0.9)) 0.05 0.0))

(defn chance-to-develop-agriculture [tribe prosperity]
  (let [ss (semi-sedentary? tribe)
        know-agriculture (know? tribe :agriculture)]
    (if (and ss (not know-agriculture)) 0.1 0)))

(defn chance-to-become-sedentary [tribe prosperity]
  (let [ss (semi-sedentary? tribe)
        know-agriculture (know? tribe :agriculture)]
    (if (and ss know-agriculture) 0.1 0)))

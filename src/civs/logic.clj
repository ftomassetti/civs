(ns
  ^{:author ftomassetti}
  civs.logic
  (:import [civs.model Population Tribe Game])
  (:require
    [civs.model :refer :all]
    [civs.logic.basic :refer :all]
    [civs.logic.tribe-choices :refer :all]
    [civs.logic.demographics :refer :all]))

(import '(java.util Random))
(import '(com.github.lands Biome))

(require '[civs.model :as model])

(defn generate-game [world n-tribes]
  (let [ game (create-game world)
         game (reduce (fn [acc, _] (:game (generate-tribe acc))) game (repeat n-tribes :just_something))]
    game))

(defn- clean-game
  "Remove dead tribes"
  [game]
  (let [tribes-map         (.tribes game)
        updated-tribes-map (select-keys tribes-map (for [[id tribe] tribes-map :when (alive? tribe)] id))]
    (assoc game :tribes updated-tribes-map)))

(defn turn [game]
  (let [ tribes (vals (.tribes game))]
    (clean-game (reduce (fn [acc t] (tribe-turn acc t)) game tribes))))

;(repeatedly 100 (fn [] (def g (turn g))))

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
         game (reduce (fn [acc, _] (:game (generate-tribe acc))) game (repeat 1 n-tribes))]
    game))

(defn turn [game]
  (let [ tribes (val (.tribes game))]
    (reduce (fn [acc t] (tribe-turn acc t)) game tribes)))
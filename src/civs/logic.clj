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
  (let [tribes (repeatedly n-tribes #(generate-tribe world))]
    (Game. world tribes)))

(defn turn [game]
  (let [ w (.world game)
         ts (map #(tribe-turn w %) (.tribes game))]
  (assoc game :tribes ts)))
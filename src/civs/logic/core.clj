(ns
  ^{:author ftomassetti}
  civs.logic.core
  (:require
    [civs.model.core :refer :all]
    [civs.logic.basic :refer :all]
    [civs.logic.tribe-choices :refer :all]
    [civs.logic.demographics :refer :all]
    [civs.model.politic :refer :all]))

; Asked question about this function:
; http://stackoverflow.com/questions/25632388/clojure-executing-an-operation-n-times-using-the-output-as-input-of-next-operat
(defn generate-game [world n-groups]
  (let [ game0 (create-game world)
         game (nth (iterate #(:game (generate-tribe %)) game0) n-groups :just_something)]
    game))

(defn- remove-dead-groups
  "Remove dead groups"
  [game]
  (for [id (political-entities-ids game)]
    (update-political-entity game id
      (fn [pe game]
        (assoc pe :groups (filter alive? (:groups pe)))))))

(defn turn [game]
  (let [groups (groups game)]
    (remove-dead-groups (reduce (fn [acc t] (tribe-turn acc t)) game groups))))

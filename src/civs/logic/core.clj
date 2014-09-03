(ns
  ^{:author ftomassetti}
  civs.logic.core
  (:require
    [civs.model.core :refer :all]
    [civs.logic.basic :refer :all]
    [civs.logic.tribe-choices :refer :all]
    [civs.logic.demographics :refer :all]))

; Asked question about this function:
; http://stackoverflow.com/questions/25632388/clojure-executing-an-operation-n-times-using-the-output-as-input-of-next-operat
(defn generate-game [world n-groups]
  (let [ game0 (create-game world)
         game (nth (iterate #(:game (generate-tribe %)) game0) n-groups :just_something)]
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

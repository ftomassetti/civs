(ns
  ^{:author ftomassetti}
  civs.model
  (:require [clojure.math.combinatorics :as combo]))

; ###########################################################
;  Generic
; ###########################################################

(defn in? [coll target] (some #(= target %) coll))

; ###########################################################
;  Population
; ###########################################################

(defrecord Population [children young-men young-women old-men old-women])

(defn total-persons [pop]
  (+ (:children pop) (:young-men pop) (:young-women pop) (:old-men pop) (:old-women pop)))

(defn active-persons [pop]
  (+ (:young-men pop) (:young-women pop)))

; ###########################################################
;  Culture
; ###########################################################

; The culture defines the behavior and beliefs of a population
; nomadism can be :nomadic, :semi-sedentary or :sedentary

; To become :semi-sedentary the population must be in a very good spot
; To develop agriculture a population must be :semi-sedentary
; To become :sedentary a population must know agriculture

(defn sedentary? [t]
  (= :sedentary (-> t .culture .nomadism)))

(defn semi-sedentary? [t]
  (= :semi-sedentary (-> t .culture .nomadism)))

(defn nomadic? [t]
  (= :nomadic (-> t .culture .nomadism)))

(defrecord Culture [nomadism knowledge])

(def initial-culture (Culture. :nomadic []))

; ###########################################################
;  Tribe
; ###########################################################

(defrecord Tribe [id name position population culture])

(defn is-dead? [tribe]
  (= 0 (total-persons (:population tribe))))

(defn alive? [tribe]
  (not (is-dead? tribe)))

(defn know? [tribe knowledge]
  (in? (-> tribe .culture .knowledge) knowledge))

(defn learn [tribe knowledge]
  (let [old-knowledge (-> tribe .culture .knowledge)
        new-knowledge (conj old-knowledge knowledge)
        new-culture (assoc (-> tribe .culture) :knowledge new-knowledge)]
    (assoc tribe :culture new-culture)))

; ###########################################################
;  World
; ###########################################################

(defn load-world [filename]
  (let [f (java.io.File. filename)]
    (. com.github.lands.PickleSerialization loadWorld f)))

(defn isLand [world pos]
  (not (.get (.getOcean world) (:x pos) (:y pos))))

(defn biome-at [world pos]
  (.get (.getBiome world) (:x pos) (:y pos)))

(defn game-width [game]
  (-> game .world .getDimension .getWidth))

(defn game-height [game]
  (-> game .world .getDimension .getHeight))

(defn inside? [world pos]
  (let [x (:x pos)
        y (:y pos)
        w (-> world .getDimension .getWidth)
        h (-> world .getDimension .getHeight)]
    (and
      (>= x 0)
      (>= y 0)
      (< x w)
      (< y h))))

(defn check-valid-position [world pos]
  (when (or (nil? (:x pos)) (nil? (:y pos)))
    (throw (Exception. (str "Invalid position given " pos))))
  (when (not (inside? world pos))
    (throw (Exception. (str "Invalid position given " pos)))))

(defn cells-around [world pos radius]
  (check-valid-position world pos)
  (let [ x (:x pos)
         y (:y pos)
         r (range (* -1 radius) (+ 1 radius))
        deltas (combo/cartesian-product r r)
        cells (map (fn [d] {:x (+ x (nth d 0)) :y (+ y (nth d 1))}) deltas)]
    (filter #(inside? world %) cells)))

(defn land-cells-around [world pos radius]
  (check-valid-position world pos)
  (filter #(isLand world %) (cells-around world pos radius)))

; ###########################################################
;  Town
; ###########################################################

(defrecord Town [id name position owner])

; ###########################################################
;  Game
; ###########################################################

(defrecord Game [world tribes towns next_id])

(defn create-game [world]
  (Game. world {} {} 1))

(defn create-tribe
  "Return the game, updated and the new tribe"
  [game name position population culture]
  (let [tribe-id (:next_id game)
        new-tribe (Tribe. tribe-id name position population culture)
        tribes (assoc (:tribes game) tribe-id new-tribe)
        game (assoc game :next_id (inc tribe-id))
        game (assoc game :tribes tribes)]
    {:game game :tribe new-tribe}))

(defn create-town
  "Return the game, updated and the new town"
  [game name position owner]
  (let [id (:next_id game)
        new-town (Town. id name position owner)
        towns (assoc (:towns game) id new-town)
        game (assoc game :next_id (inc id))
        game (assoc game :towns towns)]
    {:game game :town new-town}))

(defn get-tribe [game id]
  (get (:tribes game) id))

(defn get-town [game id]
  (get (:towns game) id))

(defn ghost-city? [game town-id]
  (let [town (get-town game town-id)
        owner (.owner town)
        tribe (get-tribe game owner)]
    (alive? tribe)))

(defn update-tribe
  "Return the game, updated"
  [game tribe]
  (let [tribe-id (:id tribe)
        tribes (assoc (:tribes game) tribe-id tribe)]
    (assoc game :tribes tribes)))

; TODO
;(defn remove-dead-tribes [game])

(defn game-total-pop [game]
  (reduce + 0 (map #(-> % .population total-persons) (vals (.tribes game)))))

(defn tribes [game]
  (vals (.tribes game)))

(defn n-tribes-alive [game]
  (.size (filter alive? (tribes game))))

(defn towns [game]
  (vals (.towns game)))

(defn n-ghost-cities [game]
  (.size (filter #(ghost-city? game (.id %)) (towns game))))

(defn game-total-pop-in-pos [game pos]
  (reduce + 0 (map #(-> % .population total-persons) (filter #(= pos (.position %)) (tribes game)))))
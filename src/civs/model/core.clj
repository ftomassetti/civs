(ns
  ^{:author ftomassetti}
  civs.model.core
  (:require [clojure.math.combinatorics :as combo]
            [civs.model.language]))

; ###########################################################
;  Generic
; ###########################################################

(defn in? [coll target] (some #(= target %) coll))

; ###########################################################
;  Population
; ###########################################################

(defrecord Population [children young-men young-women old-men old-women])

(defn total-persons [pop]
  (int (+ (:children pop) (:young-men pop) (:young-women pop) (:old-men pop) (:old-women pop))))

(defn active-persons [pop]
  (int (+ (:young-men pop) (:young-women pop))))

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

(defrecord Culture [nomadism knowledge language])

(def initial-culture (Culture. :nomadic [] nil))

; ###########################################################
;  Group (lecagy name: Tribe)
; ###########################################################

(defrecord Tribe [id name position population culture society])

(defn dead? [group]
  (= 0 (total-persons (:population group))))

(def ^:deprecated is-dead? dead?)

(defn alive? [group]
  (not (dead? group)))

(defn know? [group knowledge]
  (in? (-> group .culture .knowledge) knowledge))

(defn learn [group knowledge]
  (let [old-knowledge (-> group .culture .knowledge)
        new-knowledge (conj old-knowledge knowledge)
        new-culture (assoc (-> group .culture) :knowledge new-knowledge)]
    (assoc group :culture new-culture)))

(defn group-total-pop [group]
  (-> group :population total-persons))

(def ^:deprecated tribe-total-pos group-total-pop)

(defn get-language [group]
  (-> group .culture .language))


(defn assoc-language [group language]
  (let [old-culture (.culture group)
        new-culture (assoc old-culture :language language)]
    (assoc group :culture new-culture)))

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

(defn temperature-at [world pos]
  (.get (.getTemperature world) (:x pos) (:y pos)))

(defn humidity-at [world pos]
  (.get (.getHumidity world) (:x pos) (:y pos)))

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
;  Settlement
; ###########################################################

(defrecord Settlement [id name foundation-turn position owner])

; ###########################################################
;  Game
; ###########################################################

(defrecord Game [world tribes settlements next_id])

(defn create-game [world]
  (Game. world {} {} 1))

(defn create-tribe
  "Return the game, updated and the new tribe"
  [game name position population culture society]
  (let [tribe-id (:next_id game)
        new-tribe (Tribe. tribe-id name position population culture society)
        tribes (assoc (:tribes game) tribe-id new-tribe)
        game (assoc game :next_id (inc tribe-id))
        game (assoc game :tribes tribes)]
    {:game game :tribe new-tribe}))

(defn create-settlement
  "Return the game, updated and the new settlement"
  [game name position owner foundation-time]
  (let [id (:next_id game)
        new-town (Settlement. id name foundation-time position owner)
        settlements (assoc (:settlements game) id new-town)
        game (assoc game :next_id (inc id))
        game (assoc game :settlements settlements)]
    {:game game :settlement new-town}))

(defn get-group [game id]
  (get (:tribes game) id))

(defn group-in-pos [game pos]
  (let [groups (filter #(= pos (.position %)) (groups game))]
    (when (> (.size groups) 1)
      (throw (Exception. "More than one group in the same position")))
    (if (empty? groups)
      nil
      (first groups))))

(defn pos-free? [game pos]
  (= nil (group-in-pos game pos)))

(def ^:deprecated get-tribe get-group)

(defn get-settlement [game id]
  (get (:settlements game) id))

(defn get-settlements-owned-by [game group-id]
  (filter #(= group-id (:owner %)) (:settlements game)))

(defn ghost-city? [game settlement-id]
  (let [settlement (get-settlement game settlement-id)
        owner (.owner settlement)
        tribe (get-tribe game owner)]
    (if (nil? tribe)
      true
      (not (alive? tribe)))))

(defn update-group
  "Return the game, updated"
  [game tribe]
  (let [tribe-id (:id tribe)
        tribes (assoc (:tribes game) tribe-id tribe)]
    (assoc game :tribes tribes)))

(def ^:deprecated update-tribe update-group)

(defn update-settlement
  "Return the game, updated"
  [game settlement]
  (let [settlement-id (:id settlement)
        settlements (assoc (:settelements game) settlement-id settlement)]
    (assoc game :settlements settlements)))

(defn update-settlements
  [game settlements]
  (reduce (fn [acc s] (update-settlement acc s)) game settlements))

(defn groups-ids-in-game [game]
  (into #{} (keys (:tribes game))))

(defn game-total-pop [game]
  (reduce + 0 (map #(-> % .population total-persons) (vals (.tribes game)))))

(defn groups [game]
  (vals (.tribes game)))

(def ^:deprecated tribes groups)

(defn groups-alive [game]
  (filter alive? (groups game)))

(defn n-groups-alive [game]
  (.size (groups-alive game)))

(defn settlements [game]
  (let [s (vals (.settlements game))]
    (if (nil? s)
      []
      s)))

(defn n-ghost-cities [game]
  (.size (filter #(ghost-city? game (.id %)) (settlements game))))

(defn game-total-pop-in-pos [game pos]
  (reduce + 0 (map #(-> % .population total-persons) (filter #(= pos (.position %)) (groups game)))))
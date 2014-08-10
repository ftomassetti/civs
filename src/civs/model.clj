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

(defn isLand [world pos]
  (not (.get (.getOcean world) (:x pos) (:y pos))))

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

(defn cells-around [world pos radius]
  (let [ x (:x pos)
         y (:y pos)
         r (range (* -1 radius) (+ 1 radius))
        deltas (combo/cartesian-product r r)
        cells (map (fn [d] {:x (+ x (nth d 0)) :y (+ y (nth d 1))}) deltas)]
    (filter #(inside? world %) cells)))

(defn land-cells-around [world pos radius]
  (filter #(isLand world %) (cells-around world pos radius)))

; ###########################################################
;  Game
; ###########################################################

(defrecord Game [world tribes next_id])

(defn create-game [world]
  (Game. world {} 1))

(defn create-tribe
  "Return the game, updated and the new tribe"
  [game name position population culture]
  (let [tribe-id (:next_id game)
        new-tribe (Tribe. tribe-id name position population culture)
        tribes (assoc (:tribes game) tribe-id new-tribe)
        game (assoc game :next_id (inc tribe-id))
        game (assoc game :tribes tribes)]
    {:game game :tribe new-tribe}))

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

(defn n-tribes-alive [game]
  (.size (filter alive? (:tribes game))))

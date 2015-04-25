(ns
  ^{:author ftomassetti}
  civs.model.society
  (:require [civs.model.core :refer :all]
            [civs.model.language :refer :all]
            [civs.logic.basic :refer :all])
  (:import [civs.model.core Population Group Culture Game Population Settlement PoliticalEntity]))

; See http://en.wikipedia.org/wiki/Tribe
;     http://www.newworldencyclopedia.org/entry/Tribe
;
; Societies evolve through these steps:
;
; 1) :band         Gatherer-hunter bands, which are generally egalitarian.
; 2) :tribe        Tribal societies in which there are some limited instances of social rank and prestige.
; 3) :chiefdom     Stratified tribal societies led by chieftains (see Chiefdom).
; 4) :civilization Civilizations, with complex social hierarchies and organized, institutional governments.
;
; A Clan can be seen as a kind of tribes and can arrive to 7-10K individuals
;
; A Chiefdom can include different villages
;
; A Civilization will have different forms
;
; =============================================
; Band society
; =============================================
;
; See http://en.wikipedia.org/wiki/Band_society
;
; * it is the initial kind of society
; * it can be not larger than 100 individuals
;
; A band tend to split when growing above 100 individuals
; A band can evolve into a tribe by either joining an existing tribe or
; evolving autonomously into a tribe (it must have at least 120 individuals)
;
; The band society cannot develop agriculture or become sedentary. It can become semi-sedentary
; (considered in tribe_choices)

(def initial-society :band)

(defn- set-society [game group society-value]
  {:pre [(instance? Group group)]
   :post [(instance? Game %) (= society-value (society % group))]}
  (let [pe (to-political-entity game group)
        game (update-political-entity game (.id pe) (fn [pe game] (assoc pe :society society-value)))]
    game))

(defn band-society? [game group]
  (= :band (society game group)))

(defn tribe-society? [game group]
  (= :tribe (society game group)))

(defn chiefdom-society? [game group]
  (= :chiefdom (society game group)))

(defn evolve-in-tribe
  "The group evolve into a tribe"
  [game group]
  {:pre [(instance? Group group)]
   :post [(instance? Game %) (tribe-society? % group)]}
  (set-society game group :tribe))

(defn evolve-in-chiefdom
  [game group]
  {:pre [(instance? Group group)]
   :post [(instance? Game %) (chiefdom-society? % group)]}
  (set-society game group :chiefdm))

(defn possibility-of-evolving-into-tribe [game group]
  (if (band-society? game group)
    (let [pop (group-total-pop group)
          surplus (- pop 45)]
      (if (pos? surplus)
        (saturate (/ (float surplus) 250.0) 0.75)
        0.0))
    0.0))

(defn possibility-of-evolving-into-chiefdom [game group]
  (if (tribe-society? game group)
    (let [pop (group-total-pop group)
          surplus (- pop 900)]
      (if (> pop 900)
        0.1
        0.0))
    0.0))

(defn possibility-of-splitting [game group]
  (cond
    (band-society? game group)
      (let [pop (group-total-pop group)
            surplus (- pop 70)]
        (if (> pop 70)
          (saturate (* surplus 0.015) 0.80)
          0.02))
    (tribe-society? game group)
    (let [pop (group-total-pop group)
          surplus (- pop 900)]
      (if (> pop 900)
        (saturate (* surplus 0.002) 0.40)
        0.01))
    (chiefdom-society? game group)
    (let [pop (group-total-pop group)
          surplus (- pop 9000)]
      (if (> pop 9000)
        (saturate (* surplus 0.0002) 0.25)
        0.01))
    ; default
    :else 0.01))

(defn n-bands-alive [game]
  (.size (filter #(and (alive? %) (band-society? game %)) (groups game))))

(defn n-tribes-alive [game]
  (.size (filter #(and (alive? %) (tribe-society? game %)) (groups game))))

(defn n-chiefdoms-alive [game]
  (.size (filter #(and (alive? %) (chiefdom-society? game %)) (groups game))))
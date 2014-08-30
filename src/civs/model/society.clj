(ns
  ^{:author ftomassetti}
  civs.model.society
  (:require [civs.model.core :refer :all]
            [civs.model.language :refer :all]
            [civs.logic.basic :refer :all]))

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
; * it can be no larger than 100 individuals
;
; A band tend to split when growing above 100 individuals
; A band can evolve into a tribe by either joining an existing tribe or
; evolving autonomously into a tribe (it must have at least 120 individuals)
;
; The band society cannot develop agriculture or become sedentary. It can become semi-sedentary
; (considered in tribe_choices)

(def initial-society :band)

(defn band-society? [tribe]
  (= :band (.society tribe)))

(defn tribe-society? [tribe]
  (= :tribe (.society tribe)))

(defn chiefdom-society? [tribe]
  (= :chiefdom (.society tribe)))

; TODO assign names to all the settlements
; TODO when creating settlements for a group with a language they get a name
(defn develop-a-language [group]
  (let [group (assoc-language group (generate-language))
        language (get-language group)
        group (assoc group :name (.name language))]
    group))

(defn evolve-in-tribe
  "The group evolve into a tribe and develop a language"
  [tribe] (develop-a-language (assoc tribe :society :tribe)))

(defn evolve-in-chiefdom [tribe]
  (assoc tribe :society :chiefdom))

(defn possibility-of-evolving-into-tribe [tribe]
  (if (band-society? tribe)
    (let [pop (tribe-total-pop tribe)
          surplus (- pop 45)]
      (if (> surplus 0)
        (saturate (/ surplus 250.0) 0.75)
        0.0))
    0.0))

(defn possibility-of-evolving-into-chiefdom [tribe]
  (if (tribe-society? tribe)
    (let [pop (tribe-total-pop tribe)
          surplus (- pop 900)]
      (if (> pop 900)
        0.1
        0.0))
    0.0))

;(defn possibility-of-developing-agriculture [tribe])

(defn possibility-of-splitting [tribe]
  (cond
    (band-society? tribe)
      (let [pop (tribe-total-pop tribe)
            surplus (- pop 70)]
        (if (> pop 70)
          (saturate (* surplus 0.015) 0.80)
          0.02))
    (tribe-society? tribe)
    (let [pop (tribe-total-pop tribe)
          surplus (- pop 900)]
      (if (> pop 900)
        (saturate (* surplus 0.002) 0.40)
        0.01))
    (chiefdom-society? tribe)
    (let [pop (tribe-total-pop tribe)
          surplus (- pop 9000)]
      (if (> pop 9000)
        (saturate (* surplus 0.0002) 0.25)
        0.01))
    ; default
    :else 0.01))

(defn n-bands-alive [game]
  (.size (filter #(and (alive? %) (band-society? %)) (tribes game))))

(defn n-tribes-alive [game]
  (.size (filter #(and (alive? %) (tribe-society? %)) (tribes game))))

(defn n-chiefdoms-alive [game]
  (.size (filter #(and (alive? %) (chiefdom-society? %)) (tribes game))))
(ns
  ^{:author ftomassetti}
  civs.model)

(defn in? [coll target] (some #(= target %) coll))

(defrecord Population [children young-men young-women old-men old-women])

(defn total-persons [pop]
  (+ (:children pop) (:young-men pop) (:young-women pop) (:old-men pop) (:old-women pop)))

; The culture defines the behavior and beliefs of a population
; nomadism can be :nomadic, :semi-sedentary or :sedentary

; To become :semi-sedentary the population must be in a very good spot
; To develop agriculture a population must be :semi-sedentary
; To become :sedentary a population must know agriculture

(defrecord Culture [nomadism knowledge])

(def initial-culture (Culture. :nomadic []))

(defrecord Tribe [name position population culture])

(defn is-dead? [tribe]
  (= 0 (total-persons (:population tribe))))

(defn know? [tribe knowledge]
  (in? (-> tribe .culture .knowledge) knowledge))

(defn set-knowledge [culture knowledge]
  (Culture. (.nomadism culture) knowledge))

(defn learn [tribe knowledge]
  (let [old-knowledge (-> tribe .culture .knowledge)
        new-knowledge (conj old-knowledge knowledge)
        new-culture (set-knowledge (-> tribe .culture) new-knowledge)]
    (Tribe. (.name tribe) (.position tribe) (.population tribe) new-culture)))


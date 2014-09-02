(ns
  ^{:author ftomassetti}
  civs.logic.globals)

; ======================================
; Turn
; ======================================

(def current-turn nil)

(defn get-current-turn []
  current-turn)

(defn inc-current-turn []
  (def current-turn (inc (get-current-turn))))

(defn reset-current-turn []
  (def current-turn 0))

; ======================================
; Facts
; ======================================

; List of facts for the current turn
(def facts (atom []))

(defn fact
  "Record a new fact"
  [type params]
  (swap! facts conj (assoc params :type type)))
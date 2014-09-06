(ns
  ^{:author ftomassetti}
  civs.model.politic)

;===============================================
; Govern forms
;===============================================

; :band
; :tribe
; :chiefdom
; :kingdom
; :republic

;===============================================
; Political entity
;===============================================

(defrecord PoliticalEntity [id name govern-form groups])
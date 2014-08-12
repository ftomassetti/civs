(ns civs.logic.demographics-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model :refer :all]
            [civs.logic :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all])
  (:import [civs.model Population Tribe Town]))

(def w77 (load-world "examples-worlds/seed_77.world"))
; x 100 y 100 TUNDRA
(def pos-tundra {:x 100 :y 100})
; x 200 y 200 FOREST
(def pos-forest {:x 200 :y 200})
; x 300 y 300 JUNGLE
(def pos-jungle {:x 300 :y 300})
; x 222 y 222 SAND_DESERT
(def pos-sand-desert {:x 222 :y 222})

; id name position population culture
(deftest test-base-prosperity
  (let [t (Tribe. nil nil nil nil initial-culture nil)]
    (= 0.2 (base-prosperity w77 t pos-tundra))
    (= 0.8 (base-prosperity w77 t pos-forest))
    (= 0.8 (base-prosperity w77 t pos-jungle))
    (= 0.3 (base-prosperity w77 t pos-sand-desert)))
  (let [t (learn (Tribe. nil nil nil nil initial-culture nil) :agriculture)]
    (= 0.2 (base-prosperity w77 t pos-tundra))
    (= 0.8 (base-prosperity w77 t pos-forest))
    (= 0.8 (base-prosperity w77 t pos-jungle))
    (= 0.3 (base-prosperity w77 t pos-sand-desert))))

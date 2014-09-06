(ns civs.logic.demographics-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model.core :refer :all]
            [civs.logic.core :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all])
  (:import [civs.model.core Population Settlement]))

(def w77 (load-world "examples-worlds/seed_77.world"))
; x 100 y 100 TUNDRA
(def pos-tundra {:x 100 :y 100})
; x 200 y 200 FOREST
(def pos-forest {:x 200 :y 200})
; x 300 y 300 JUNGLE
(def pos-jungle {:x 300 :y 300})
; x 222 y 222 SAND_DESERT
(def pos-sand-desert {:x 222 :y 222})



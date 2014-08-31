(ns civs.acceptance-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model.society :refer :all]
            [civs.model.core :refer :all]
            [civs.logic.core :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all]))

(import '(java.util Random))

; Here we just check nothing really weird happens and some basic
; expectations are met in a few random cases

(def w77 (load-world "examples-worlds/seed_77.world"))

(def verbose-acceptance-tests true)

(defn mock-crand-int []
  (let [my-r (Random. 1)]
    (fn [n] (.nextInt my-r n))))

(defn mock-crand-float []
  (let [my-r (Random. 1)]
    (fn [] (.nextFloat my-r))))

(defn mock-land-biome [desired-biome]
  (fn [world pos] (if (isLand world pos) desired-biome com.github.lands.Biome/OCEAN)))

(defn mock-prosperity-temperature-multiplier-at []
  (fn [world pos] 1.0))

(defn mock-prosperity-humidity-multiplier-at []
  (fn [world pos] 1.0))

(deftest test-not-everyone-dies-immediately
  (with-redefs [crand-int   (mock-crand-int)
                crand-float (mock-crand-float)]
    (let [g0 (generate-game w77 10)
          g (reduce (fn [g _] (turn g)) g0 (repeat 10 :_))]
      (is (> (game-total-pop g) 0)))))

(deftest test-population-do-not-expand-too-much-too-fast
  (with-redefs [crand-int   (mock-crand-int)
                crand-float (mock-crand-float)]
    (let [g0 (generate-game w77 10)
          start-pop (game-total-pop g0)
          g (reduce (fn [g _] (turn g)) g0 (repeat 10 :_))]
      (is (and (< (game-total-pop g) (* start-pop 2)))))))

; We do not want to drop too drammatically and it should not increase too much

(defn check-biome [biome min-factor max-factor ntribes nturns]
  (with-redefs [ biome-at (mock-land-biome biome)
                 prosperity-temperature-multiplier-at (mock-prosperity-temperature-multiplier-at)
                 prosperity-humidity-multiplier-at (mock-prosperity-humidity-multiplier-at)
                 crand-int   (mock-crand-int)
                 crand-float (mock-crand-float)]
    (let [g0 (generate-game w77 ntribes)
          start-pop (game-total-pop g0)
          g (reduce (fn [g _] (turn g)) g0 (repeat nturns :_))
          final-pop (game-total-pop g)]
      (when verbose-acceptance-tests
        (println (.toString biome) "scenario: initial population" start-pop)
        (println (.toString biome) "scenario: final population"   final-pop "after" nturns "turns"))
      (is (> final-pop (* start-pop min-factor)))
      (is (< final-pop (* start-pop max-factor))))))

(deftest test-population-in-a-sand-desert
  (check-biome com.github.lands.Biome/SAND_DESERT 0.5 1.3 30 10))

; We do not want to drop too drammatically and it should not increase too much
(deftest test-population-in-a-grassland
  (check-biome com.github.lands.Biome/GRASSLAND 0.9 1.8 30 10))

; We do not want to drop too drammatically and it should not increase too much
(deftest test-population-in-a-savanna
  (check-biome com.github.lands.Biome/SAVANNA 0.9 1.5 30 10))

; We do not want to drop too drammatically and it should not increase too much
(deftest test-population-in-a-forest
  (check-biome com.github.lands.Biome/FOREST 0.9 1.6 30 10))

; We do not want to drop too drammatically and it should not increase too much
(deftest test-population-in-a-jungle
  (check-biome com.github.lands.Biome/JUNGLE 0.9 1.7 30 10))

(deftest test-long-lasting-population-in-grassland
  (check-biome com.github.lands.Biome/GRASSLAND 2.0 1000.0 50 100))

(deftest test-long-lasting-population-in-savanna
  (check-biome com.github.lands.Biome/SAVANNA 0.8 10.0 50 100))

(def game-scenario-w77-100tribes-30turns
  (with-redefs [ crand-int   (mock-crand-int)
                 crand-float (mock-crand-float)]
    (let [ ntribes 100
           nturns  30
           g0 (generate-game w77 ntribes)
           g (reduce (fn [g _] (turn g)) g0 (repeat nturns :_))]
      g)))

(deftest test-some-societies-remain-nomadic
  (let [ g game-scenario-w77-100tribes-30turns
         societies                   (groups-alive g)
         nsocieties-total            (.size societies)
         nsocieties-still-nomadic    (.size (filter nomadic? societies))
         nsocieties-semi-sedentary   (.size (filter semi-sedentary? societies))
         nsocieties-sedentary        (.size (filter sedentary? societies))]
    (when verbose-acceptance-tests
      (println "scenario-w77-100tribes-30turns nomadic" nsocieties-still-nomadic)
      (println "scenario-w77-100tribes-30turns semi-sedentary" nsocieties-semi-sedentary)
      (println "scenario-w77-100tribes-30turns sedentary" nsocieties-sedentary))
    (is (and (>= nsocieties-still-nomadic 10) (<= nsocieties-still-nomadic 60)))
    ))

(deftest test-some-societies-become-semi-sedentary
  (let [ g game-scenario-w77-100tribes-30turns
         societies                   (groups-alive g)
         nsocieties-total            (.size societies)
         nsocieties-still-nomadic    (.size (filter nomadic? societies))
         nsocieties-semi-sedentary   (.size (filter semi-sedentary? societies))
         nsocieties-sedentary        (.size (filter sedentary? societies))]
    (is (and (>= nsocieties-semi-sedentary 10) (<= nsocieties-semi-sedentary 55)))
    ))

(deftest test-some-societies-become-sedentary
  (let [ g game-scenario-w77-100tribes-30turns
         societies                   (groups-alive g)
         nsocieties-total            (.size societies)
         nsocieties-still-nomadic    (.size (filter nomadic? societies))
         nsocieties-semi-sedentary   (.size (filter semi-sedentary? societies))
         nsocieties-sedentary        (.size (filter sedentary? societies))]
    (is (and (>= nsocieties-sedentary 3) (<= nsocieties-sedentary 15)))))

(deftest test-some-discover-agriculture
  (let [ g game-scenario-w77-100tribes-30turns
         societies                   (groups-alive g)
         nsocieties-agriculture      (.size (filter #(know? % :agriculture) societies))]
    (when verbose-acceptance-tests
      (println "scenario-w77-100tribes-30turns agriculture" nsocieties-agriculture))
    (is (and (>= nsocieties-agriculture 3) (<= nsocieties-agriculture 25)))))

(deftest test-some-most-do-not-discover-agriculture
  (let [ g game-scenario-w77-100tribes-30turns
         societies                   (groups-alive g)
         nsocieties-no-agriculture   (.size (filter #(not (know? % :agriculture)) societies))]
    (when verbose-acceptance-tests
      (println "scenario-w77-100tribes-30turns no agriculture" nsocieties-no-agriculture))
    (is (and (>= nsocieties-no-agriculture 35) (<= nsocieties-no-agriculture 90)))))

(deftest test-some-societies-are-band
  (let [ g game-scenario-w77-100tribes-30turns
         societies                   (groups-alive g)
         target                      (.size (filter band-society? societies))]
    (when verbose-acceptance-tests
      (println "scenario-w77-100tribes-30turns band" target))
    (is (and (>= target 10) (<= target 85)))))

(deftest test-some-societies-are-tribe
  (let [ g game-scenario-w77-100tribes-30turns
         societies                   (groups-alive g)
         target                      (.size (filter tribe-society? societies))]
    (when verbose-acceptance-tests
      (println "scenario-w77-100tribes-30turns tribe" target))
    (is (and (>= target 5) (<= target 35)))))

(deftest test-no-societies-are-yet-chiefdom
  (let [ g game-scenario-w77-100tribes-30turns
         societies                   (groups-alive g)
         target                      (.size (filter chiefdom-society? societies))]
    (is (= 0 target))))

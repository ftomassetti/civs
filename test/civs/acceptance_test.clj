(ns civs.acceptance-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model :refer :all]
            [civs.logic :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all]))

; Here we just check nothing really weird happens and some basic
; expectations are met in a few random cases

(def w77 (load-world "examples-worlds/seed_77.world"))

(def verbose-acceptance-tests true)

(deftest test-not-everyone-dies
  (let [g0 (generate-game w77 10)
        g (reduce (fn [g _] (turn g)) g0 (repeat 10 :_))]
    (is (> (game-total-pop g) 0))))

(deftest test-population-do-not-expand-too-much-too-fast
  (let [g0 (generate-game w77 10)
        start-pop (game-total-pop g0)
        g (reduce (fn [g _] (turn g)) g0 (repeat 10 :_))]
    (is (< (game-total-pop g) (* start-pop 2)))))

(defn mock-land-biome [desired-biome]
  (fn [world pos] (if (isLand world pos) desired-biome com.github.lands.Biome/OCEAN)))

; We do not want to drop too drammatically and it should not increase too much
(deftest test-population-in-a-sand-desert
  (with-redefs [biome-at (mock-land-biome com.github.lands.Biome/SAND_DESERT)]
    (let [g0 (generate-game w77 30)
          start-pop (game-total-pop g0)
          g (reduce (fn [g _] (turn g)) g0 (repeat 10 :_))
          final-pop (game-total-pop g)]
      (when verbose-acceptance-tests
        (println "sand-desert scenario: initial population " start-pop)
        (println "sand-desert scenario: final population "   final-pop))
      (is (> final-pop (* start-pop 0.5)))
      (is (< final-pop (* start-pop 1.3))))))

; We do not want to drop too drammatically and it should not increase too much
(deftest test-population-in-a-grassland
  (with-redefs  [biome-at (mock-land-biome com.github.lands.Biome/GRASSLAND)]
    (let [g0 (generate-game w77 30)
          start-pop (game-total-pop g0)
          g (reduce (fn [g _] (turn g)) g0 (repeat 10 :_))
          final-pop (game-total-pop g)]
      (when verbose-acceptance-tests
        (println "grassland scenario: initial population " start-pop)
        (println "grassland scenario: final population "   final-pop))
      (is (> final-pop (* start-pop 0.9)))
      (is (< final-pop (* start-pop 1.7))))))

; We do not want to drop too drammatically and it should not increase too much
(deftest test-population-in-a-savanna
  (with-redefs  [biome-at (mock-land-biome com.github.lands.Biome/SAVANNA)]
    (let [g0 (generate-game w77 30)
          start-pop (game-total-pop g0)
          g (reduce (fn [g _] (turn g)) g0 (repeat 10 :_))
          final-pop (game-total-pop g)]
      (when verbose-acceptance-tests
        (println "savanna scenario: initial population " start-pop)
        (println "savanna scenario: final population "   final-pop))
      (is (> final-pop (* start-pop 0.9)))
      (is (< final-pop (* start-pop 1.5))))))

; We do not want to drop too drammatically and it should not increase too much
(deftest test-population-in-a-forest
  (with-redefs  [biome-at (mock-land-biome com.github.lands.Biome/FOREST)]
    (let [g0 (generate-game w77 30)
          start-pop (game-total-pop g0)
          g (reduce (fn [g _] (turn g)) g0 (repeat 10 :_))
          final-pop (game-total-pop g)]
      (when verbose-acceptance-tests
        (println "forest scenario: initial population " start-pop)
        (println "forest scenario: final population "   final-pop))
      (is (> final-pop (* start-pop 0.9)))
      (is (< final-pop (* start-pop 1.6))))))

; We do not want to drop too drammatically and it should not increase too much
(deftest test-population-in-a-jungle
  (with-redefs  [biome-at (mock-land-biome com.github.lands.Biome/JUNGLE)]
    (let [g0 (generate-game w77 30)
          start-pop (game-total-pop g0)
          g (reduce (fn [g _] (turn g)) g0 (repeat 10 :_))
          final-pop (game-total-pop g)]
      (when verbose-acceptance-tests
        (println "jungle scenario: initial population " start-pop)
        (println "jungle scenario: final population "   final-pop))
      (is (> final-pop (* start-pop 0.9)))
      (is (< final-pop (* start-pop 1.7))))))
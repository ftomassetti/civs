(ns civs.model-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model.core :refer :all]
            [civs.model.language :refer :all]
            [civs.model.society :refer :all]
            [civs.logic.core :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all])
  (:import [civs.model.core Population Group Settlement]))

(def w77 (load-world "examples-worlds/seed_77.world"))

(deftest test-total-persons
  (is (= 15 (total-persons (Population. 1 2 3 4 5)))))

(deftest test-active-persons
  (is (= 5 (active-persons (Population. 1 2 3 4 5)))))

(deftest test-is-dead?
  (is (= false (is-dead? (Group. nil nil nil (Population. 1 2 3 4 5) nil))))
  (is (= true  (is-dead? (Group. nil nil nil (Population. 0 0 0 0 0) nil)))))

(deftest test-alive?
  (is (= true  (alive?  (Group. nil nil nil (Population. 1 2 3 4 5) nil))))
  (is (= false (alive?  (Group. nil nil nil (Population. 0 0 0 0 0) nil)))))

(deftest test-game-width
  (let [g (create-game w77)]
    (is (= 512 (game-width g)))))

(deftest test-game-height
  (let [g (create-game w77)]
    (is (= 512 (game-height g)))))

(deftest test-create-tribe
  (let [g (create-game nil)
        g (:game (create-tribe g "Group1" nil nil nil nil))
        t (get (-> g .groups) 2)]
  (is t)
  (is (= 2 (.id t)))))

(deftest test-cells-around
  (is (= '({:x 4, :y 7} {:x 4, :y 8} {:x 4, :y 9} {:x 5, :y 7} {:x 5, :y 8} {:x 5, :y 9} {:x 6, :y 7} {:x 6, :y 8} {:x 6, :y 9})
        (cells-around w77 {:x 5 :y 8} 1))))

(deftest test-cells-around-near-borders
  (is (= '( {:x 0, :y 0} {:x 0, :y 1} {:x 1, :y 0} {:x 1, :y 1})
        (cells-around w77 {:x 0 :y 0} 1))))

(deftest test-land-cells-around-in-the-ocean
  ; We are in a corner, surrounded just by see
  (is '() (land-cells-around w77 {:x 0 :y 0} 5)))

(deftest test-land-cells-around-in-land
  ; This tile is land
  (is '({:x 100, :y 100}) (land-cells-around w77 {:x 100 :y 100} 0)))

(deftest test-create-tribe
  (let [initial-g (create-game nil)
        res (create-tribe initial-g "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society)
        g (:game res)
        t (:tribe res)]
    ; there should be one tribe in the world
    (is (= 1 (.size (tribes g))))
    (is (= 1 (.id t)))
    (is (= "name" (.name t)))
    (is (= {:x 15 :y 18} (.position t)))
    (is (= (Population. 1 2 3 4 5) (.population t)))
    (is (= initial-culture (culture g t)))))

(deftest test-create-settlement
  (let [initial-g (create-game nil)
        res (create-settlement initial-g "name" {:x 15 :y 18} :123 456)
        g (:game res)
        t (:settlement res)]
    ; there should be one town in the world
    (is (= 1 (.size (settlements g))))
    (is (= 1 (.id t)))
    (is (= "name" (.name t)))
    (is (= {:x 15 :y 18} (.position t)))
    (is (= :123 (.owner t)))
    (is (= 456 (.foundation-turn t)))))

(deftest test-game-total-pop
  (let [g0 (create-game nil)
        g1 (:game (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society))
        g2 (:game (create-tribe g1 "name" {:x 15 :y 18} (Population. 0 1 2 0 0) initial-culture initial-society))]
    (is (= 0 (game-total-pop g0)))
    (is (= 15 (game-total-pop g1)))
    (is (= 18 (game-total-pop g2)))))

(deftest test-get-group
  (let [g0 (create-game nil)
        g1 (:game (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society))]
    (is (= nil (get-group g0 1)))
    (is (= (Group. 2 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) 1) (get-group g1 2)))))

(deftest test-get-settlement
  (let [g0 (create-game nil)
        g1 (:game (create-settlement g0 "name" {:x 15 :y 18} :123 10))]
    (is (= nil (get-settlement g0 1)))
    (is (= (Settlement. 1 "name" 10 {:x 15 :y 18} :123) (get-settlement g1 1)))))

(deftest test-ghost-city
  (let [g0 (create-game nil)
        g1 (:game (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society))
        g2 (:game (create-settlement g1 "name" {:x 15 :y 18} 1 11))
        g3 (:game (create-tribe g2 "name" {:x 15 :y 18} (Population. 0 0 0 0 0) initial-culture initial-society))
        g4 (:game (create-settlement g3 "name" {:x 15 :y 18} 3 12))]
    (is (= false (ghost-city? g2 3)))
    (is (= true (ghost-city? g4 6)))))

(deftest test-n-groups-alive
  (let [g0 (create-game nil)
        g1 (:game (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society))
        g2 (:game (create-tribe g1 "name" {:x 15 :y 18} (Population. 0 0 0 0 0) initial-culture initial-society))]
    (is (= 0 (n-groups-alive g0)))
    (is (= 1 (n-groups-alive g1)))
    (is (= 1 (n-groups-alive g2)))))

(deftest test-update-group
  (let [g0 (create-game nil)
        g1 (:game (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society))
        g2 (update-group g1 (Group. 1 "name2" {:x 25 :y 28} (Population. 0 1 1 0 0) nil))]
    (is (= (Group. 1 "name"  {:x 15 :y 18} (Population. 1 2 3 4 5) nil) (get-group g1 1)))
    (is (= (Group. 1 "name2" {:x 25 :y 28} (Population. 0 1 1 0 0) nil) (get-group g2 1)))))

(deftest test-n-ghost-cities
  (let [g0 (create-game nil)
        g1 (:game (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society))
        g2 (:game (create-settlement g1 "name" {:x 15 :y 18} 1 20))
        g3 (:game (create-tribe g2 "name" {:x 15 :y 18} (Population. 0 0 0 0 0) initial-culture initial-society))
        g4 (:game (create-settlement g3 "name" {:x 15 :y 18} 3 21))]
    (is (= 0 (n-ghost-cities g0)))
    (is (= 0 (n-ghost-cities g2)))
    (is (= 1 (n-ghost-cities g4)))))

(deftest test-assoc-and-use-language
  (let [ ga (create-game nil)
         {gr :group, ga :game} (create-group ga nil nil nil initial-culture initial-society)
         pe (by-id ga (.political-entity-id gr))
         pe (assoc-language ga pe (generate-language))
         l  (get-language ga pe)
         n  (.name l)]
    (is (not (nil? n)))
    (is (not (.isEmpty n)))))
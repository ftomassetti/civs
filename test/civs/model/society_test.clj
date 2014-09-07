(ns civs.model.society-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model.core :refer :all]
            [civs.logic.core :refer :all]
            [civs.model.society :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all])
  (:import [civs.model.core Population Settlement]))

(def w77 (load-world "examples-worlds/seed_77.world"))

(deftest a-small-band-should-have-a-low-probability-of-splitting
  (let [ g (create-game nil)
         {t :tribe g :game} (create-tribe g "unnamed" {:x 15 :y 18} (Population. 1 1 1 1 1) initial-culture :band)
         p (possibility-of-splitting g t)]
    (is (< p 0.05))))

(deftest a-very-large-band-should-have-a-high-probability-of-splitting
  (let [ g (create-game nil)
         {t :tribe g :game} (create-tribe g "unnamed" {:x 15 :y 18} (Population. 40 40 40 40 40) initial-culture :band)
         p (possibility-of-splitting g t)]
    (is (> p 0.50))))

(deftest a-small-tribe-should-have-a-low-probability-of-splitting
  (let [ g (create-game nil)
         {t :tribe g :game} (create-tribe g "unnamed" {:x 15 :y 18} (Population. 15 15 15 15 15) initial-culture :tribe)
         p (possibility-of-splitting g t)]
    (is (< p 0.05))))

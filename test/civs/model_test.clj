(ns civs.model-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model :refer :all]
            [civs.logic :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all]))

(deftest testCreateTribe
  (let [g (create-game nil)
        g (:game (create-tribe g "Tribe1" nil nil nil))
        t (get (-> g .tribes) 1)]
  (is t)
  (is (= 1 (.id t)))))
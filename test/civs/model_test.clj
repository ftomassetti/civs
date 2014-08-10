(ns civs.model-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model :refer :all]
            [civs.logic :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all])
  (:import [civs.model Population Tribe]))

(deftest test-total-persons
  (is (= 15 (total-persons (Population. 1 2 3 4 5)))))

(deftest test-active-persons
  (is (= 5 (active-persons (Population. 1 2 3 4 5)))))

(deftest test-is-dead?
  (is (= false (is-dead? (Tribe. nil nil nil (Population. 1 2 3 4 5) nil))))
  (is (= true  (is-dead? (Tribe. nil nil nil (Population. 0 0 0 0 0) nil)))))

(deftest test-alive?
  (is (= true  (alive?  (Tribe. nil nil nil (Population. 1 2 3 4 5) nil))))
  (is (= false (alive?  (Tribe. nil nil nil (Population. 0 0 0 0 0) nil)))))

(deftest test-create-tribe
  (let [g (create-game nil)
        g (:game (create-tribe g "Tribe1" nil nil nil))
        t (get (-> g .tribes) 1)]
  (is t)
  (is (= 1 (.id t)))))
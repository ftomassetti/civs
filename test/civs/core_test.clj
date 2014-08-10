(ns civs.core-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model :refer :all]
            [civs.logic :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all]))

(def g (generate-game w 1))
(def t (:tribe (generate-tribe g)))

(deftest langGenerationWorks
  (let [l (com.github.langgen.SamplesBasedLanguageFactory/getRandomLanguage)]
    (is (not (empty? (.name l))))))

(deftest loadWorld
  (let [filename "examples-worlds/seed_77.world"
        f (java.io.File. filename)
        w (. com.github.lands.PickleSerialization loadWorld f)]
    (is (= "seed_77" (.getName w)))))

(deftest testSplitBy
  (is (= [3 7] (split-by 10 0.3))))

(deftest testRSplitByWithFactorZero
  (is (= [0 100] (rsplit-by 100 0.0))))

(deftest testRSplitByWithFactorOne
  (is (= [100 0] (rsplit-by 100 1.0))))

(deftest testKnow?
   (is (not (know? t :agriculture))))

(deftest testLearn
  (let [t (learn t :agriculture)]
    (is (know? t :agriculture))))

(deftest testCellsAround
  (is (= '({:x 4, :y 7} {:x 4, :y 8} {:x 4, :y 9} {:x 5, :y 7} {:x 5, :y 8} {:x 5, :y 9} {:x 6, :y 7} {:x 6, :y 8} {:x 6, :y 9})
    (cells-around w {:x 5 :y 8} 1))))

(deftest testCellsAroundNearBorders
  (is (= '( {:x 0, :y 0} {:x 0, :y 1} {:x 1, :y 0} {:x 1, :y 1})
    (cells-around w {:x 0 :y 0} 1))))
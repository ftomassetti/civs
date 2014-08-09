(ns civs.core-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model :refer :all]
            [civs.logic :refer :all]))

(def t (generate-tribe w))

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
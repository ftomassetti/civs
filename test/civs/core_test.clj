(ns civs.core-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model.core :refer :all]
            [civs.logic.core :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all]))

(def w77 (load-world "examples-worlds/seed_77.world"))
(def g (generate-game w77 1))

(deftest test-lang-generation-works
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

(deftest test-know?
  (let [{t :group g :game} (generate-tribe g)]
   (is (not (know? g t :agriculture)))))

(deftest test-learn
  (let [ {t :group g :game} (generate-tribe g)
         pe (to-political-entity g t)
         g (learn g t :agriculture)]
    (is (know? g t :agriculture))))

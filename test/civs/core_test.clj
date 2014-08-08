(ns civs.core-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]))

(deftest langGenerationWorks
  (let [l (com.github.langgen.SamplesBasedLanguageFactory/getRandomLanguage)]
    (is (not (empty? (.name l))))))

(deftest loadWorld
  (let [filename "examples-worlds/seed_77.world"
        f (java.io.File. filename)
        w (. com.github.lands.PickleSerialization loadWorld f)]
    (is (= "seed_77" (.getName w)))))

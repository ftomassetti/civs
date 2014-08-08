(ns civs.core-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]))

(deftest langGenerationWorks
  (let [l (com.github.langgen.SamplesBasedLanguageFactory/getRandomLanguage)]
    (is (not (empty? (.name l))))))

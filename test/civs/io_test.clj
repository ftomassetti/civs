(ns civs.io-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model :refer :all]
            [civs.io :refer :all]
            [civs.logic :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all]))

(def w77 (load-world "examples-worlds/seed_77.world"))

(deftest test-dir-contains
  (is (= true (dir-contains? "examples-worlds" "seed_77.world")))
  (is (= false (dir-contains? "examples-worlds" "unexisting_file"))))

(deftest test-absolute-filename
  (is (= true (absolute-filename? "/root/some-path")))
  (is (= true (absolute-filename? "C:\\some-path")))
  (is (= false (absolute-filename? "some-path")))
  (is (= false (absolute-filename? "../some-path"))))

(deftest test-dir-resolver
  (is (= "examples-worlds/seed_77.world" ((dir-lists-resolver ["unexisting1" "examples-worlds" "unexisting2"]) "seed_77.world")))
  (is (= "examples-worlds/seed_77.world" ((dir-lists-resolver ["unexisting1" "" "unexisting2"]) "examples-worlds/seed_77.world")))
  (is (= nil ((dir-lists-resolver ["unexisting1" "unexisting2"]) "seed_77.world"))))

(deftest test-serialization
  (let [g (generate-game w77 5)
        simulation-result (simulate g 10 false)
        ser-str (to-serialized-str simulation-result {:world-filename "examples-worlds/seed_77.world"})
        loaded (from-serialized-str ser-str {:resolver (dir-lists-resolver [""])})]
    (is (= loaded simulation-result))))

; (require '[miner.tagged :as tag])
; (require '[clojure.edn :as edn])
;(is (= (edn/read-string {:default tag/tagged-default-reader}
; (pr-str data))))

;(defn my-reader [tag value]
;  (println "TAG " tag)
;  (tag/tagged-default-reader tag value))

;(defn check-serialization [data]
;  (=
;    data
;    (edn/read-string {:default my-reader} (pr-str data))))
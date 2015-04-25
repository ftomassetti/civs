(ns civs.model.core-test
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

; ###########################################################
;  Generic
; ###########################################################

(deftest test-in?
  (is (true? (in? [1 2 3] 1)))
  (is (false? (in? [1 2 3] 4)))
  (is (true? (in? (list 1 2 3) 1)))
  (is (false? (in? (list 1 2 3) 4))))

; ###########################################################
;  Population
; ###########################################################

(deftest test-total-persons
  (is (= 15 (total-persons (Population. 1 2 3 4 5)))))

(deftest test-active-persons
  (is (= 5 (active-persons (Population. 1 2 3 4 5)))))

; ###########################################################
;  Culture
; ###########################################################

(defn- consider-base-group [f]
  (let [ga (create-game w77)
        {ga :game gr :group} (create-tribe ga "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society)]
    (f ga gr)))

(deftest test-get-and-set-nomadism
  (consider-base-group
    (fn [ga gr]
      (let [ga (update-nomadism ga gr :nomadic)]
        (is (nomadic? ga gr))
        (is (not (semi-sedentary? ga gr)))
        (is (not (sedentary? ga gr))))
      (let [ga (update-nomadism ga gr :semi-sedentary)]
        (is (not (nomadic? ga gr)))
        (is (semi-sedentary? ga gr))
        (is (not (sedentary? ga gr))))
      (let [ga (update-nomadism ga gr :sedentary)]
        (is (not (nomadic? ga gr)))
        (is (not (semi-sedentary? ga gr)))
        (is (sedentary? ga gr))))))

; ###########################################################
;  Group
; ###########################################################

(deftest test-group-to-political-entity
  (consider-base-group
    (fn [ga gr]
      (let [pe (to-political-entity ga gr)]
        (is (= 1 (.id pe)))))))

(deftest test-culture
  (consider-base-group
    (fn [ga gr]
      (is (= initial-culture (culture ga gr))))))

(deftest test-society
  (consider-base-group
    (fn [ga gr]
      (is (= initial-society (society ga gr))))))

(deftest test-dead?
  (is (false? (dead? (Group. nil nil nil (Population. 1 2 3 4 5) nil))))
  (is (true?  (dead? (Group. nil nil nil (Population. 0 0 0 0 0) nil)))))

(deftest test-alive?
  (is (true?  (alive?  (Group. nil nil nil (Population. 1 2 3 4 5) nil))))
  (is (false? (alive?  (Group. nil nil nil (Population. 0 0 0 0 0) nil)))))

(deftest test-know?
  (consider-base-group
    (fn [g _]
      (let [{t :group g :game} (generate-tribe g)]
        (is (not (know? g t :agriculture)))))))

(deftest test-learn
  (consider-base-group
    (fn [g _]
      (let [ {t :group g :game} (generate-tribe g)
             pe (to-political-entity g t)
             g (learn g t :agriculture)]
        (is (know? g t :agriculture))))))

(deftest test-group-total-pop
  (let [g0 (create-game nil)
        g1 (:group (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society))
        g2 (:group (create-tribe g0 "name" {:x 15 :y 18} (Population. 0 1 2 0 0) initial-culture initial-society))]
    (is (= 15 (group-total-pop g1)))
    (is (= 3 (group-total-pop g2)))))

(deftest test-get-language
  (consider-base-group
    (fn [ga gr]
      (is (nil? (get-language ga gr))))))

(deftest test-assoc-language
  (consider-base-group
    (fn [ga gr]
      (let [l (generate-language)
            pe0 (by-id ga 1)
            pe1 (assoc-language ga pe0 l)]
        (is (= l (get-language ga pe1)))))))

; ###########################################################
;  Political entity
; ###########################################################

(deftest test-political-entity-to-political-entity
  (consider-base-group
    (fn [ga gr]
      (let [ pe0 (by-id ga 1)
             pe (to-political-entity ga pe0)]
        (is (= pe pe0))))))

(deftest test-game-width
  (let [g (create-game w77)]
    (is (= 512 (game-width g)))))

(deftest test-game-height
  (let [g (create-game w77)]
    (is (= 512 (game-height g)))))

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
    ; there should be one group in the world
    (is (= 1 (.size (groups g))))
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
    (is (zero? (game-total-pop g0)))
    (is (= 15 (game-total-pop g1)))
    (is (= 18 (game-total-pop g2)))))

(deftest test-get-group
  (let [g0 (create-game nil)
        g1 (:game (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society))]
    (is (nil? (get-group g0 1)))
    (is (= (Group. 2 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) 1) (get-group g1 2)))))

(deftest test-get-settlement
  (let [g0 (create-game nil)
        g1 (:game (create-settlement g0 "name" {:x 15 :y 18} :123 10))]
    (is (nil? (get-settlement g0 1)))
    (is (= (Settlement. 1 "name" 10 {:x 15 :y 18} :123) (get-settlement g1 1)))))

(deftest test-ghost-city
  (let [g0 (create-game nil)
        {g1 :game gr1 :group} (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society)
        {g2 :game s1 :settlement} (create-settlement g1 "name" {:x 15 :y 18} (.id gr1) 11)
        {g3 :game gr2 :group} (create-tribe g2 "name" {:x 15 :y 18} (Population. 0 0 0 0 0) initial-culture initial-society)
        {g4 :game s2 :settlement} (create-settlement g3 "name" {:x 15 :y 18} (.id gr2) 12)]
    (is (false? (ghost-city? g2 (.id s1))))
    (is (true? (ghost-city? g4 (.id s2))))))

(deftest test-n-groups-alive
  (let [g0 (create-game nil)
        g1 (:game (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society))
        g2 (:game (create-tribe g1 "name" {:x 15 :y 18} (Population. 0 0 0 0 0) initial-culture initial-society))]
    (is (zero? (n-groups-alive g0)))
    (is (= 1 (n-groups-alive g1)))
    (is (= 1 (n-groups-alive g2)))))

(deftest test-update-group
  (let [g0 (create-game nil)
        {g1 :game gr1 :group} (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society)
        g2 (update-group g1 (Group. (.id gr1) "name2" {:x 25 :y 28} (Population. 0 1 1 0 0) 123))]
    (is (= (Group. (.id gr1) "name"  {:x 15 :y 18} (Population. 1 2 3 4 5) 1) (get-group g1 (.id gr1))))
    (is (= (Group. (.id gr1) "name2" {:x 25 :y 28} (Population. 0 1 1 0 0) 123) (get-group g2 (.id gr1))))))

(deftest test-n-ghost-cities
  (let [g0 (create-game nil)
        {g1 :game gr1 :group} (create-tribe g0 "name" {:x 15 :y 18} (Population. 1 2 3 4 5) initial-culture initial-society)
        {g2 :game s1 :settlement} (create-settlement g1 "name" {:x 15 :y 18} (.id gr1) 20)
        {g3 :game gr2 :group} (create-tribe g2 "name" {:x 15 :y 18} (Population. 0 0 0 0 0) initial-culture initial-society)
        {g4 :game s2 :settlement} (create-settlement g3 "name" {:x 15 :y 18} (.id gr2) 21)]
    (is (zero? (n-ghost-cities g0)))
    (is (zero? (n-ghost-cities g2)))
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

(deftest test-by-id
  (let [g0 (create-game nil)
        gr (Group. 123 nil nil nil nil)
        g1 (update-group g0 gr)]
    (is (= (by-id g1 123) gr))))
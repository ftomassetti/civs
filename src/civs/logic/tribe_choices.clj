(ns
  ^{:author ftomassetti}
  civs.logic.tribe-choices
  (:require
    [civs.model :refer :all]
    [civs.logic :refer :all]
    [civs.logic.demographics :refer :all])
  (:import [civs.model Population Tribe]))

(defn chance-to-become-semi-sedentary [tribe prosperity]
  (if (and (nomadic? tribe) (> prosperity 0.9)) 0.05 0.0))

(defn chance-to-develop-agriculture [tribe prosperity]
  (let [ss (semi-sedentary? tribe)
        know-agriculture (know? tribe :agriculture)]
    (if (and ss (not know-agriculture)) 0.1 0.0)))

(defn chance-to-become-sedentary [tribe prosperity]
  (let [ss (semi-sedentary? tribe)
        know-agriculture (know? tribe :agriculture)]
    (if (and ss know-agriculture) 0.1 0.0)))

(defrecord PossibleEvent [name chance apply])

(def become-semi-sedentary
  (PossibleEvent.
    :become-semi-sedentary
    chance-to-become-semi-sedentary
    (fn [tribe]
      (let [new-culture (assoc (.culture tribe) :nomadism :semi-sedentary)]
        {
          :tribe (assoc tribe :culture new-culture)
          :params {}
          :msg "became semi-sedentary"
          }))))

(def discover-agriculture
  (PossibleEvent.
    :discover-agriculture
    chance-to-develop-agriculture
    (fn [tribe]
      (let [new-culture (assoc (.culture tribe) :nomadism :sedentary)]
        {
          :tribe (learn tribe :agriculture)
          :params {}
          :msg "discover agriculture"
          }))))

(def become-sedentary
  (PossibleEvent.
    :become-sedentary
    chance-to-become-sedentary
    (fn [tribe]
      (let [new-culture (assoc (.culture tribe) :nomadism :sedentary)]
        {
          :tribe (assoc tribe :culture new-culture)
          :params {}
          :msg "became sedentary"
          }))))

(defn consider-event [tribe prosperity event]
  (let [p ((.chance event) tribe prosperity)]
    (if (roll p)
      (let [apply-res ((.apply event) tribe)
            new-tribe (:tribe apply-res)
            params (assoc (:params apply-res) :tribe new-tribe)
            msg (:msg apply-res)]
        (fact (:name event) params msg)
        new-tribe)
      tribe)))

(defn consider-events [tribe prosperity events]
  (if (empty? events)
    tribe
    (let [e (first events)
          re (rest events)]
      (consider-events (consider-event tribe prosperity e) prosperity re))))

(defn consider-all-events [tribe prosperity]
  (consider-events tribe prosperity [become-semi-sedentary discover-agriculture become-sedentary]))

(defn turn [world tribe]
  (let [p (prosperity world tribe)]
    (update-population world
      (consider-all-events tribe p))))
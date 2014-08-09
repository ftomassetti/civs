(ns
  ^{:author ftomassetti}
  civs.logic.tribe-choices
  (:require
    [civs.model :refer :all]
    [civs.logic.basic :refer :all]
    [civs.logic.demographics :refer :all])
  (:import [civs.model Population Tribe]))

(defn chance-to-become-semi-sedentary [world tribe]
  (let [prosperity (prosperity world tribe)]
    (if (and (nomadic? tribe) (> prosperity 0.9)) 0.05 0.0)))

(defn chance-to-develop-agriculture [world tribe]
  (let [prosperity (prosperity world tribe)
         ss (semi-sedentary? tribe)
        know-agriculture (know? tribe :agriculture)]
    (if (and ss (not know-agriculture)) 0.1 0.0)))

(defn chance-to-become-sedentary [world tribe]
  (let [prosperity (prosperity world tribe)
         ss (semi-sedentary? tribe)
        know-agriculture (know? tribe :agriculture)]
    (if (and ss know-agriculture) 0.1 0.0)))

(defrecord PossibleEvent [name chance apply])

(def become-semi-sedentary
  (PossibleEvent.
    :become-semi-sedentary
    chance-to-become-semi-sedentary
    (fn [world tribe]
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
    (fn [world tribe]
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
    (fn [world tribe]
      (let [new-culture (assoc (.culture tribe) :nomadism :sedentary)]
        {
          :tribe (assoc tribe :culture new-culture)
          :params {}
          :msg "became sedentary"
          }))))

(def migrate
  (PossibleEvent.
    :migrate
    (fn [world tribe]
      (case
        (sedentary? tribe) 0
        (semi-sedentary? tribe) 0.15
        (nomadic? tribe) 0.85))
    (fn [world tribe]
      (let [ pos (.position tribe)
             possible-destinations (land-cells-around world pos 3)
             preferences (map (fn [pos] {
                                     :preference (perturbate-low (prosperity-in-pos world tribe pos))
                                     :pos pos
                                     }) possible-destinations)
             preferences (sort-by :preference preferences)
             target (:pos (first preferences))]
        {
          :tribe (assoc tribe :position target)
          :params {:to target}
          :msg "migrate"
          }))))

(defn consider-event [world tribe event]
  (let [p ((.chance event) world tribe)]
    (if (roll p)
      (let [apply-res ((.apply event) world tribe)
            new-tribe (:tribe apply-res)
            params (assoc (:params apply-res) :tribe new-tribe)
            msg (:msg apply-res)]
        (fact (:name event) params msg)
        new-tribe)
      tribe)))

(defn consider-events [world tribe events]
  (if (empty? events)
    tribe
    (let [e (first events)
          re (rest events)]
      (consider-events world (consider-event world tribe e) re))))

(defn consider-all-events [world tribe]
  (consider-events world tribe [become-semi-sedentary discover-agriculture become-sedentary migrate]))

(defn tribe-turn [world tribe]
  (let [p (prosperity world tribe)]
    (update-population world
      (consider-all-events world tribe))))

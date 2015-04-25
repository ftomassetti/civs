(ns
  ^{:author ftomassetti}
  civs.logic.tribe-choices
  (:require
    [civs.model.core :refer :all]
    [civs.model.language :refer :all]
    [civs.model.society :refer :all]
    [civs.logic.basic :refer :all]
    [civs.logic.globals :refer :all]
    [civs.logic.demographics :refer :all])
  (:import [civs.model.core Population Game]))

(defn- migration-radius [game group]
  (cond
    (nomadic? game group) 2
    (semi-sedentary? game group) 2
    (sedentary? game group) 4
    :default (throw (Exception. "Unexpected"))))

(defn- discovery-population-factor [total-pop required-pop]
  (+ 1.0 (Math/log10 (/ (float total-pop) required-pop))))

(defn chance-to-become-semi-sedentary [game tribe]
  {:pre [(not (nil? tribe))]}
  (let [ world (.world game)
         prosperity (prosperity game tribe)]
    (if (and (nomadic? game tribe) (> prosperity 0.6))
      (saturate (/ (- prosperity 0.75) 1.5) 0.10)
      0.0)))

; Must be at least a tribe society
(defn chance-to-develop-agriculture [game group]
  {:pre [(not (nil? group))]}
  (if (and
        (not (nomadic? game group))
        (not (know? game group :agriculture))
        (not (band-society? game group)))
    (let [agr-prosperity (base-prosperity-per-activity (.world game) (.position group) :agriculture)
          prob (* (- agr-prosperity 0.75) 6.0)
          prob (* (discovery-population-factor (group-total-pop group) 200) prob)
          prob (saturate (max 0.0 prob) 0.30)]
      ; agriculture is discovered in places good for agriculture
      prob)
    0.0))

; Must be at least a tribe society
(defn chance-to-become-sedentary [game tribe]
  {:pre [(not (nil? tribe))]}
  (let [world (.world game)
         prosperity (prosperity game tribe)
         ss (semi-sedentary? game tribe)
        know-agriculture (know? game tribe :agriculture)]
    (if (and
          ss
          know-agriculture
          (not (band-society? game tribe)))
            (if (> prosperity 0.72)
              (saturate (/ (- prosperity 0.75) 1.5) 0.20)
              0.0) 0.0)))

(defrecord PossibleEvent [name chance apply])

(def become-semi-sedentary
  (PossibleEvent.
    :become-semi-sedentary
    chance-to-become-semi-sedentary
    (fn [game tribe]
      (let [game (update-nomadism game tribe :semi-sedentary)]
        {
          :game game
          :params {}
          }))))

(def discover-agriculture
  (PossibleEvent.
    :discover-agriculture
    chance-to-develop-agriculture
    (fn [game tribe]
        {
          :game (learn game (to-political-entity game tribe) :agriculture)
          :params {}
          })))

(def become-sedentary
  (PossibleEvent.
    :become-sedentary
    chance-to-become-sedentary
    (fn [game tribe]
      (let [ pos (.position tribe)
             game (update-nomadism game tribe :sedentary)
             language (get-language game tribe)
             settlement-name (if (nil? language) :unnamed (.name language))]
        {
          :game (:game (create-settlement game settlement-name pos (:id tribe) current-turn))
          :params {}
          }))))

(def migrate
  (PossibleEvent.
    :migrate
    (fn [game group]
      (let [ world (.world game)
             pos (.position group)
             possible-destinations (filter #(pos-free? game %) (land-cells-around world pos (migration-radius game group)))]
        (if (empty? possible-destinations)
          0.0
          (let [p (prosperity-in-pos game group pos)
                ip (- 1.0 p)]
            (* ip (case
              (sedentary? game group) 0
              (semi-sedentary? game group) 0.15
              (nomadic? game group) 0.85))))))
    (fn [game group]
      (let [ world (.world game)
             pos (.position group)
             _ (check-valid-position world pos)
             possible-destinations (filter #(pos-free? game %) (land-cells-around world pos (migration-radius game group)))
             preferences (map (fn [pos] {
                                     :preference (perturbate-low (prosperity-in-pos game group pos))
                                     :pos pos
                                     }) possible-destinations)
             preferences (sort-by :preference preferences)
             target (:pos (first preferences))]
        {
          :group (assoc group :position target)
          :params {:to target}
          }))))

(defn- split-pop [population]
  (let [[rc,lc] (rsplit-by (.children population) 0.4)
        [rym,lym] (rsplit-by (.young-men population) 0.4)
        [ryw,lyw] (rsplit-by (.young-women population) 0.4)
        [rom,lom] (rsplit-by (.old-men population) 0.4)
        [row,low] (rsplit-by (.old-women population) 0.4)]
    {:remaining (Population. rc rym ryw rom row) :leaving (Population. lc lym lyw lom low)}))

(def split
  (PossibleEvent.
    :split
    (fn [game group]
      (let [c (crowding game group)
            pop (-> group .population total-persons)
            world (.world game)
            pos (.position group)
            possible-destinations (filter #(pos-free? game %) (land-cells-around world pos (migration-radius game group)))]
        (if
          (and (> pop 35) (< c 0.9) (seq possible-destinations))
          (/ (opposite c) 2.7)
          0.0)))
    (fn [game group]
      (let [ world (.world game)
             pos (.position group)
             sp (split-pop (.population group))
             possible-destinations (filter #(pos-free? game %) (land-cells-around world pos (migration-radius game group)))
             preferences (map (fn [pos] {
                                          :preference (perturbate-low (prosperity-in-pos game group pos))
                                          :pos pos
                                          }) possible-destinations)
             preferences (sort-by :preference preferences)
             dest-target (:pos (first preferences))
             language (get-language game group)
             new-group-name (if (nil? language) :unnamed (.name language))
             res (create-tribe game new-group-name dest-target (:leaving sp) (culture game group) (society game group))
             game (:game res)]
        (if (sedentary? game group)
          (let [settlement-name (if (nil? language) :unnamed (.name language))
                game (:game (create-settlement game settlement-name dest-target (:id (:group res)) current-turn))]
            {
              :game game
              :group (assoc group :population (:remaining sp))
              :params {}
            })
          {
            :game game
            :group (assoc group :population (:remaining sp))
            :params {}
            })))))

(defn develop-a-language
  "Return a game"
  [game group-id]
  (let [group (get-group game group-id)
        game (assoc-language-in-game game (to-political-entity game group) (generate-language))
        language (get-language game group)
        _ (assert (not (nil? language)))
        group (assoc group :name (.name language))
        game (update-group game group)
        settlements (get-settlements-owned-by game (:id group))
        settlements (map #(assoc % :name (.name language)) settlements)
        game (update-settlements game settlements)]
    game))

(def evolution-in-tribe
  (PossibleEvent.
    :evolve-in-tribe
    possibility-of-evolving-into-tribe
    (fn [game group]
      (let [game (develop-a-language game (:id group))
            group (get-group game (:id group))
            game (evolve-in-tribe game group)]
        {
          :game game
          :params {}
          }))))

(def evolution-in-chiefdom
  (PossibleEvent.
    :evolve-in-chiefdom
    possibility-of-evolving-into-chiefdom
    (fn [game tribe]
      {
        :group (evolve-in-chiefdom tribe)
        :params {}
        })))

(defn consider-event [game tribe event]
  {:pre [game tribe event]
   :post [(:game %) (:group %)]}
  "Return a map of game and tribe, changed"
  (let [p ((.chance event) game tribe)]
    (if (roll p)
      (let [apply-res ((.apply event) game tribe)
            new-tribe (:group apply-res)
            new-game (or (:game apply-res) game)
            new-game (if new-tribe (update-group new-game new-tribe) new-game)
            new-tribe (or new-tribe (by-id new-game (.id tribe)))
            group-id (.id new-tribe)
            params (assoc (:params apply-res) :group group-id)]
        (fact (:name event) params)
        {:game new-game :group new-tribe})
      {:game game :group tribe} )))

(defn consider-events
  "Return a map of game and tribe, changed"
  [game tribe events]
  {:pre [game tribe]
   :post [(:game %) (:group %)]}
  (if (empty? events)
    {:game game :group tribe}
    (let [e (first events)
          re (rest events)
          res (consider-event game tribe e)]
      (consider-events (:game res) (:group res) re))))

(defn consider-all-events
  "Return a map of game and tribe, changed"
  [game tribe]
  (consider-events game tribe [become-semi-sedentary discover-agriculture become-sedentary migrate split evolution-in-tribe evolution-in-chiefdom]))

(defn group-turn
  "Return the game, updated"
  [game tribe]
  {:pre  [(instance? Game game) (:groups game)]
   :post [(instance? Game %) (:groups %)]}
  (let [ world (.world game)
         tribe (update-population game tribe)
        game (update-group game tribe)]
    (:game (consider-all-events game tribe))))


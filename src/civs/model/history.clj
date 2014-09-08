(ns
  ^{:author ftomassetti}
  civs.model.history)

;#########################################
; Turns
;#########################################

(defn n-turns [history]
  (.size (keys (:facts history))))

(defn turns
  ([history] (sort (keys (:game-snapshots history)))))

(defn exist-turn? [history turn]
  (not (nil? (some #{turn} (turns history)))))

(defn game-at [history turn]
  (get (:game-snapshots history) turn))

(defn group-at [history turn group-id]
  (get (:groups (game-at history turn)) group-id))

(defn political-entity-at [history turn group-id]
  (get (:political-entities (game-at history turn)) group-id))


(defn update-game [history turn game]
  (let [snapshots (:game-snapshots history)
        snapshots (assoc snapshots turn game)]
    (assoc history :game-snapshots snapshots)))

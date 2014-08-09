(ns
  ^{:author ftomassetti}
  civs.model)

(defrecord Population [children young-men young-women old-men old-women])

(defn total-persons [pop]
  (+ (:children pop) (:young-men pop) (:young-women pop) (:old-men pop) (:old-women pop)))

(defrecord Tribe [name position population])

(defn is-dead? [tribe]
  (= 0 (total-persons (:population tribe))))


(ns
  ^{:author ftomassetti}
  civs.model.core
  (:require [clojure.math.combinatorics :as combo]
            [civs.model.language]))

; ###########################################################
;  Records
; ###########################################################

(defrecord PoliticalEntity [id name society groups culture])
(defrecord Game [world settlements groups political-entities next_id])
(defrecord Culture [nomadism knowledge language])
(defrecord Group [id name position population political-entity-id])
(defrecord Settlement [id name foundation-turn position owner])

(declare culture)
(declare society)
(declare update-political-entity)
(declare to-political-entity)

; ###########################################################
;  Generic
; ###########################################################

(defn in? [coll target] (not (nil? (some #(= target %) coll))))

; ###########################################################
;  Population
; ###########################################################

(defrecord Population [children young-men young-women old-men old-women])

(defn total-persons [pop]
  {:pre [(instance? Population pop)]}
  (int (+ (:children pop) (:young-men pop) (:young-women pop) (:old-men pop) (:old-women pop))))

(defn active-persons [pop]
  (int (+ (:young-men pop) (:young-women pop))))

; ###########################################################
;  Culture
; ###########################################################

; The culture defines the behavior and beliefs of a population
; nomadism can be :nomadic, :semi-sedentary or :sedentary

; To become :semi-sedentary the population must be in a very good spot
; To develop agriculture a population must be :semi-sedentary
; To become :sedentary a population must know agriculture

(defn sedentary? [game x]
  (= :sedentary (.nomadism (culture game x))))

(defn semi-sedentary? [game x]
  (= :semi-sedentary (.nomadism (culture game x))))

(defn nomadic? [game x]
  {:pre [(not (nil? x))]}
  (= :nomadic (.nomadism (culture game x))))

(defn valid-nomadism? [v]
  (in? [:nomadic :semi-sedentary :sedentary] v))

(defn update-nomadism [game x nomadism]
  {:pre [(valid-nomadism? nomadism)]
   :post [(instance? Game %)]}
  (let [pe (to-political-entity game x)]
    (update-political-entity game (.id pe) (fn [pe _] (assoc-in pe [:culture :nomadism] nomadism)))))

(def initial-culture (Culture. :nomadic [] nil))

; ###########################################################
;  Group
; ###########################################################

(declare by-id)
(declare required-by-id)

; Can get an id, a political-entity or a group
(defmulti to-political-entity (fn [_ el] (class el)))

(defmethod to-political-entity Group [game group]
  (try
    (required-by-id game (:political-entity-id group))
    (catch IllegalArgumentException e
      (throw (IllegalStateException. (str "Political entity for group " group " not found") e)))))

(defn society [game el]
  (.society (to-political-entity game el)))

(defn dead? [group]
  {:pre [(instance? Group group) (instance? Population (:population group))]}
  (zero? (total-persons (:population group))))

(defn alive? [group]
  {:pre [(instance? Group group) (instance? Population (:population group))]}
  (not (dead? group)))

(defn know? [game group knowledge]
  (in? (.knowledge (culture game group)) knowledge))

(defn learn [game x knowledge]
  {:pre [(or
          (instance? PoliticalEntity x)
          (instance? Group x))]}
  (let [ pe (to-political-entity game x)
         old-knowledge (.knowledge (culture game pe))
         new-knowledge (conj old-knowledge knowledge)
         pe (assoc-in pe [:culture :knowledge] new-knowledge)]
    (update-political-entity game pe)))

(defn group-total-pop [group]
  {:pre [(instance? Group group)]
   :post [(>= % 0)]}
  (-> group :population total-persons))

(defn get-language [game group]
  (.language (culture game group)))

(defn assoc-language [game political-entity language]
  {:pre [(not (nil? political-entity)) (instance? PoliticalEntity political-entity)]}
  (let [old-culture (culture game political-entity)
        new-culture (assoc old-culture :language language)]
    (assoc political-entity :culture new-culture)))

(defn assoc-language-in-game [game political-entity language]
  {:pre [(not (nil? political-entity)) (instance? PoliticalEntity political-entity)]
   :post [(instance? Game %)]}
  (update-political-entity game
    (assoc-language game political-entity language)))

; ###########################################################
;  World
; ###########################################################

(defn load-world [filename]
  (let [f (java.io.File. filename)]
    (com.github.lands.PickleSerialization/loadWorld f)))

(defn isLand [world pos]
  (not (.get (.getOcean world) (:x pos) (:y pos))))

(defn biome-at [world pos]
  (.get (.getBiome world) (:x pos) (:y pos)))

(defn temperature-at [world pos]
  (.get (.getTemperature world) (:x pos) (:y pos)))

(defn humidity-at [world pos]
  (.get (.getHumidity world) (:x pos) (:y pos)))

(defn game-width [game]
  (-> game .world .getDimension .getWidth))

(defn game-height [game]
  (-> game .world .getDimension .getHeight))

(defn inside? [world pos]
  (let [x (:x pos)
        y (:y pos)
        w (-> world .getDimension .getWidth)
        h (-> world .getDimension .getHeight)]
    (and
      (>= x 0)
      (>= y 0)
      (< x w)
      (< y h))))

(defn check-valid-position [world pos]
  (when (or (nil? (:x pos)) (nil? (:y pos)))
    (throw (Exception. (str "Invalid position given " pos))))
  (when-not (inside? world pos)
    (throw (Exception. (str "Invalid position given " pos)))))

(defn cells-around [world pos radius]
  (check-valid-position world pos)
  (let [ x (:x pos)
         y (:y pos)
         r (range (* -1 radius) (inc radius))
        deltas (combo/cartesian-product r r)
        cells (map (fn [d] {:x (+ x (nth d 0)) :y (+ y (nth d 1))}) deltas)]
    (filter #(inside? world %) cells)))

(defn land-cells-around [world pos radius]
  (check-valid-position world pos)
  (filter #(isLand world %) (cells-around world pos radius)))

; ###########################################################
;  Settlement
; ###########################################################

;===============================================
; Society (Govern forms)
;===============================================

; :band
; :tribe
; :chiefdom
; :kingdom
; :republic

;===============================================
; Political entity
;===============================================

(defmethod to-political-entity PoliticalEntity [game pe]
  pe)

(defn add-political-entity
  "Associate the correct id to the political-entity.
  Return the game and the assigned id"
  [game political-entity]
  {:pre [(nil? (:id political-entity))]} ;political entity should have no id, it will be assigned
  (let [id (:next_id game)
        pe (assoc political-entity :id id)
        game (assoc game :next_id (inc id))
        game (assoc-in game [:political-entities id] pe)]
    {:game game :new-id id } ))

(defn update-political-entity
  "The function f should take the political entity and the game"
  ([game id f] {:pre [(>= id 0)]} (update-in game [:political-entities id] f game))
  ([game pe]   (update-in game [:political-entities (.id pe)] (fn [_] pe))))

(defn political-entities-ids
  [game]
  (keys (:political-entities game)))

(defn add-group-to-political-entity [game pe-id group-id]
  (update-political-entity game pe-id
    (fn [pe game]
      (let [gs (.groups pe)
            gs (conj gs group-id)]
        (assoc pe :groups gs)))))

(defn- by-id-with-collection
  "Return the element associated to the id"
  [game id]
  (reduce
    #(let
       [res (get-in game [%2 id])]
       (if (nil? res)
         %1
         {:element res :collection %2}))
    nil [:groups :settlements :political-entities]))

(defn by-id
  "Return the element associated to the id"
  [game id]
  (:element (by-id-with-collection game id)))

(defn required-by-id [game id]
  (if (nil? id)
    (throw (NullPointerException. "Null id given"))
    (let [res (by-id game id)]
      (if (nil? res)
        (throw (IllegalArgumentException. (str "Invalid id given " id)))
        res))))

(defn update-by-id
  "f(game, entity)"
  [game id f]
  (let [{element :element, collection :collection} (by-id-with-collection game id)]
    (update-in game [collection id] f)))

; Return the Culture of the entity
(defn culture [game x]
  {:pre [(not (nil? x))]}
  (.culture (to-political-entity game x)))

; ###########################################################
;  Game
; ###########################################################

(defn create-game [world]
  (Game. world {} {} {} 1))

(defn create-group
  "Return the game, updated and the new tribe.
   The group creates its own independent political entity"
  [game name position population culture society]
  (let [{pe-id :new-id game :game} (add-political-entity game (PoliticalEntity. nil :unnamed society [] culture))
        tribe-id (:next_id game)
        new-tribe (Group. tribe-id name position population pe-id)
        tribes (assoc (:groups game) tribe-id new-tribe)
        game (assoc game :next_id (inc tribe-id))
        game (assoc game :groups tribes)
        game (add-group-to-political-entity game pe-id tribe-id)]
    ; :tribe is there only to support old calls
    {:game game :tribe new-tribe :group new-tribe}))

(def ^:deprecated create-tribe create-group)

(defn create-settlement
  "Return the game, updated and the new settlement"
  [game name position owner foundation-time]
  (let [id (:next_id game)
        new-town (Settlement. id name foundation-time position owner)
        settlements (assoc (:settlements game) id new-town)
        game (assoc game :next_id (inc id))
        game (assoc game :settlements settlements)]
    {:game game :settlement new-town}))

(defn get-group [game id]
  (get (:groups game) id))

(defn groups [game]
  (vals (:groups game)))

(defn group-in-pos [game pos]
  (let [groups (filter #(= pos (.position %)) (groups game))]
    (when (> (.size groups) 1)
      (throw (Exception. "More than one group in the same position")))
    (when (seq groups) (first groups))))

(defn pos-free? [game pos]
  (nil? (group-in-pos game pos)))

(def ^:deprecated get-tribe get-group)

(defn get-settlement [game id]
  (get (:settlements game) id))

(defn get-settlements-owned-by [game group-id]
  (filter #(= group-id (:owner %)) (:settlements game)))

(defn ghost-city? [game settlement-id]
  (let [settlement (get-settlement game settlement-id)
        owner (.owner settlement)
        tribe (get-tribe game owner)]
    (if (nil? tribe)
      true
      (not (alive? tribe)))))

(defn update-group
  "Return the game, updated"
  [game tribe]
  {:pre [tribe (:id tribe) (>= (:id tribe) 0)]
   :post [(by-id % (:id tribe))]}
  (assoc-in game [:groups (:id tribe)] tribe))

(def ^:deprecated update-tribe update-group)

(defn update-settlement
  "Return the game, updated"
  [game settlement]
  (let [settlement-id (:id settlement)
        settlements (assoc (:settelements game) settlement-id settlement)]
    (assoc game :settlements settlements)))

(defn update-settlements
  [game settlements]
  (reduce update-settlement game settlements))

(defn groups-ids-in-game [game]
  (set (keys (:groups game))))

(defn game-total-pop [game]
  {:pre [(instance? Game game)]
   :post [(>= % 0)]}
  (reduce + 0 (map #(-> % .population total-persons) (groups game))))

(def ^:deprecated tribes groups)

(defn groups-alive [game]
  (filter alive? (groups game)))

(defn n-groups-alive [game]
  (.size (groups-alive game)))

(defn settlements [game]
  (let [s (vals (.settlements game))]
    (if (nil? s)
      []
      s)))

(defn populated-settlements [game]
  (filter #(not (ghost-city? game (.id %))) (settlements game)))

(defn n-ghost-cities [game]
  (.size (filter #(ghost-city? game (.id %)) (settlements game))))

(defn game-total-pop-in-pos [game pos]
  (reduce + 0 (map #(-> % .population total-persons) (filter #(= pos (.position %)) (groups game)))))
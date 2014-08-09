(ns
  ^{:author ftomassetti}
  civs.logic
  (:import [civs.model Population Tribe Game])
  (:require
    [civs.model :refer :all]
    [civs.logic.basic :refer :all]))

(import '(java.util Random))
(import '(com.github.lands Biome))

(require '[civs.model :as model])

(defn randomLandPos [world]
  (let [pos (randomPos (.getDimension world))]
    (if (isLand world pos)
      pos
      (randomLandPos world))))

(defn randomInitialPopulation []
  (model/Population. (rand-int 15) (rand-int 15) (rand-int 15) (rand-int 5) (rand-int 5)))

(defn generate-tribe [world]
  (model/Tribe. :unnamed (randomLandPos world) (randomInitialPopulation) initial-culture))

(defn base-prosperity [world tribe pos]
  (let [ x (:x pos)
         y (:y pos)
         b (.get (.getBiome world) x y)]
    (if
      (know? tribe :agriculture)
      (case (.name b)
        "OCEAN" (throw (Exception. (str "No prosperity in the ocean")))
        "ICELAND"     0.1
        "TUNDRA"      0.2
        "ALPINE"      0.5
        "GLACIER"     0.05
        "GRASSLAND"   1.0
        "ROCK_DESERT" 0.3
        "SAND_DESERT" 0.3
        "FOREST"      0.8
        "SAVANNA"     0.7
        "JUNGLE"      0.8
        (throw (Exception. (str "Unknown biome" b))))
      (case (.name b)
        "OCEAN" (throw (Exception. (str "No prosperity in the ocean")))
        "ICELAND"     0.1
        "TUNDRA"      0.2
        "ALPINE"      0.5
        "GLACIER"     0.05
        "GRASSLAND"   1.0
        "ROCK_DESERT" 0.3
        "SAND_DESERT" 0.3
        "FOREST"      0.8
        "SAVANNA"     0.7
        "JUNGLE"      0.8
        (throw (Exception. (str "Unknown biome" b)))))))

(defn crowding
  "Factor which influence prosperity depending on the technology and the number of inhabitants:
  agriculture supports more inhabitants"
  [world tribe pos]
  (let [ actives (-> tribe .population active-persons)
         tot     (-> tribe .population total-persons)
         max-supportable (if (know? tribe :agriculture) 1000 150)
         pop-supportable (* actives (if (know? tribe :agriculture) 6.0 2.5))
         pop-supportable (min max-supportable pop-supportable)]
    (if (< tot pop-supportable) 1.0
      (if (or (= pop-supportable 0.0) (= tot 0))
        0.0
        (/ 1.0 (/ tot pop-supportable))))))

(defn prosperity-in-pos
  "The prosperity a tribe would have in a given position"
  [world tribe pos]
  (let [ base     (base-prosperity world tribe pos)
         crowding (crowding world tribe pos)]
    (* base crowding)))

(defn prosperity
  "A number in [0,1] whic indicates how well the tribe is living.
  For now it just depends on the kind of biome where the tribe is.
  Increase for young men and women, reduce for children and old people
  Depending on the kind of activity done (gathering/agriculture)
  certain number of people can be supported"
  [world tribe]
  (prosperity-in-pos world tribe (.position tribe)))

(defn fact [type params msg]
  (println msg))

(defn saturate [value max]
  (if (> value max)
    max
    value))

(defn generate-game [world n-tribes]
  (let [tribes (repeatedly n-tribes #(generate-tribe world))]
    (Game. world tribes)))

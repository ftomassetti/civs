(ns
  ^{:author ftomassetti}
  civs.logic
  (:import [civs.model Population Tribe]))

(import '(java.util Random))
(import '(com.github.lands Biome))

(require '[civs.model :as model])

(def r (Random. 1))

(defn rand-int [n]
  (.nextInt r n))

(defn rand-float []
  (.nextFloat r))

(defn opposite [n]
  (- 1.0 n))

(defn perturbate
  "n should be in [0,1]"
  [n factor]
  (let [perturbation (/ (- (rand-float) 0.5) factor)
        res (+ n perturbation)
        res (if (< res 0.0) 0.0 res)
        res (if (> res 1.0) 1.0 res)]
    res))

(defn perturbate-high
  "n should be in [0,1]"
  [n]
  (perturbate n 1.0))

(defn perturbate-med
  "n should be in [0,1]"
  [n]
  (perturbate n 3.0))

(defn perturbate-low
  "n should be in [0,1]"
  [n]
  (perturbate n 5.0))

(defn rand-range [a b]
  (when (> a b)
    (throw (Exception. "Invalid range")))
  (let [diff (- b a)]
    (+ a (rand-int diff))))

(defn randomPos [dimension]
  (let [x (.nextInt r (.getWidth dimension))
        y (.nextInt r (.getHeight dimension))]
  {:x x :y y}))

(defn round [v]
  (int (+ 0.5 v)))

(defn mean [a b]
  (/ (+ a b) 2.0))

(defn force-in-range [n min max]
  (when (< n min)
    (throw (Exception. "Too low")))
  (when (> n max)
    (throw (Exception. (str "Too high: limit " max " value " n)))))

(defn split-by
  "factor is in [0,1]"
  [n factor]
  (force-in-range factor 0.0 1.0)
  (let [a (round (* n factor))
        b (- n a)]
    [a b]))

(defn rsplit-by
  "factor is in [0,1]"
  [n factor]
  (force-in-range factor 0.0 1.0)
  (let
    [values (repeatedly n #(if (< (rand-float) factor) [1 0] [0 1]))]
  (reduce #(map + %1 %2) [0 0] values)))

(defn isLand [world pos]
  (not (.get (.getOcean world) (:x pos) (:y pos))))

(defn randomLandPos [world]
  (let [pos (randomPos (.getDimension world))]
    (if (isLand world pos)
      pos
      (randomLandPos world))))

(defn randomInitialPopulation []
  (model/Population. (rand-int 10) (rand-int 10) (rand-int 10) (rand-int 3) (rand-int 3)))

(defn generate-tribe [world]
  (model/Tribe. :unnamed (randomLandPos world) (randomInitialPopulation)))

;(def t (generate-tribe w))
;(def t (update-population w t))

(defn prosperity
  "A number in [0,1] whic indicates how well the tribe is living.
  For now it just depends on the kind of biome where the tribe is.
  TODO increase for young men and women, reduce for children and old people"
  [world tribe]
  (let [ x (:x (.position tribe))
         y (:y (.position tribe))
         b (.get (.getBiome world) x y)]
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
      (throw (Exception. (str "Unknown biome" b))))))

(defn fact [type params msg]
  (println msg))

(defn saturate [value max]
  (if (> value max)
    max
    value))


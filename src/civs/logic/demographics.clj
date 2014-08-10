(ns
  ^{:author ftomassetti}
  civs.logic.demographics
  (:require
    [civs.model :refer :all]
    [civs.logic.basic :refer :all])
  (:import [civs.model Population Tribe]))

(import '(java.util Random))
(import '(com.github.lands Biome))

(require '[civs.model :as model])

(defn randomLandPos [world]
  (let [pos (randomPos (.getDimension world))]
    (if (isLand world pos)
      pos
      (randomLandPos world))))

(defn randomInitialPopulation []
  (model/Population. (rand-int 12) (rand-int 12) (rand-int 12) (rand-int 4) (rand-int 4)))

(defn generate-tribe
  "Return a map game, tribe"
  [game]
  (let [world (.world game)]
    (create-tribe game :unnamed (randomLandPos world) (randomInitialPopulation) initial-culture)))

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

(defn men-availability-factor [young-men young-women]
  (let [ men-factor (/ (float young-men) young-women)
         res (/ (+ men-factor 0.5) 2)]
    (saturate (saturate res 1.0) (* men-factor 3))))

(defn update-births
  "This method returns a delta"
  [world tribe prosperity]
  (let [young-men               (-> tribe :population :young-men)
        young-women             (-> tribe :population :young-women)
        men-availability-factor (men-availability-factor young-men young-women)
        women-fertility         (* young-women 1.1 (perturbate-high prosperity))
        births                  (round (* women-fertility men-availability-factor))]
    (fact :births {:tribe tribe :n births} (str births " children were born"))
    (Population. births 0 0 0 0)))

(defn update-children
  "Children can die or grow in to young men or women.
  This method returns a delta"
  [world tribe prosperity]
  (let [mortality (* (opposite prosperity) 0.35)
        n-children (-> tribe :population :children)
        [dead, grown] (rsplit-by n-children mortality)
        [men, women] (rsplit-by grown 0.5)]
    (fact :children-dead {:tribe tribe :n dead} (str dead " children died"))
    (fact :children-grown-as-men {:tribe tribe :n men} (str men " children grew as men"))
    (fact :children-grown-as-women {:tribe tribe :n women} (str women " children grew as women"))
    (Population. (* -1 n-children) men women 0 0)))

(defn update-young-population
  "Young men and women can die, remain young or grow old.
  This method returns a delta"
  [world tribe prosperity]
  (let [mortality-men     (* (opposite prosperity) 0.25)
        mortality-women   (* (opposite prosperity) 0.25)
        n-young-men       (-> tribe :population :young-men)
        n-young-women     (-> tribe :population :young-women)
        [dead-m, alive-m] (rsplit-by n-young-men mortality-men)
        [dead-w, alive-w] (rsplit-by n-young-women mortality-women)
        [grown-m, _]      (rsplit-by alive-m 0.25)
        [grown-w, _]      (rsplit-by alive-w 0.25)]
    (fact :young-men-dead {:tribe tribe :n dead-m} (str dead-m " young men died"))
    (fact :young-women-dead {:tribe tribe :n dead-w} (str dead-w " young women died"))
    (fact :young-men-grew-old {:tribe tribe :n grown-m} (str grown-m " young men grew old"))
    (fact :young-women-grew-old {:tribe tribe :n grown-w} (str grown-w " young women grew old"))
    (Population. 0 (* -1 (+ dead-m grown-m)) (* -1 (+ dead-w grown-w)) grown-m grown-w)))

(defn update-old-population
  "Old men and women can die or remain old.
  This method returns a delta"
  [world tribe prosperity]
  (let [mortality-men   (saturate (* (opposite prosperity) 1.1) 1.0)
        mortality-women (saturate (* (opposite prosperity) 1.1) 1.0)
        n-old-men   (-> tribe :population :old-men)
        n-old-women (-> tribe :population :old-women)
        [dead-m, alive-m] (rsplit-by n-old-men mortality-men)
        [dead-w, alive-w] (rsplit-by n-old-women mortality-women)]
    (fact :old-men-dead {:tribe tribe :n dead-m} (str dead-m " old men died"))
    (fact :old-women-dead {:tribe tribe :n dead-w} (str dead-w " old women died"))
    (Population. 0 0 0 (* -1 dead-m) (* -1 dead-w))))


(defn- sum-population [pop delta]
  (let [new-children    (+ (.children pop)    (.children delta))
        new-young-men   (+ (.young-men pop)   (.young-men delta))
        new-young-women (+ (.young-women pop) (.young-women delta))
        new-old-men     (+ (.old-men pop)     (.old-men delta))
        new-old-women   (+ (.old-women pop)   (.old-women delta))]
    (Population. new-children new-young-men new-young-women new-old-men new-old-women)))

(defn update-tribe-population
  "TODO this sum the various deltas to the tribe population"
  [tribe deltas]
  (let [new-population (reduce sum-population (.population tribe) deltas)]
    (assoc tribe :population new-population)))

(defn update-population
  "We use deltas, so that we first calculated all the changes on the original
   population and only then we apply all the changes, after all the calculations
   are done, otherwise the first calculations would influence the results of the
   other steps"
  [world tribe]
  (let [p (prosperity world tribe)
        deltaFromChildren        (update-children         world tribe p)
        deltaFromYoungPopulation (update-young-population world tribe p)
        deltaFromOldPopulation   (update-old-population   world tribe p)
        deltaFromBirths          (update-births world tribe p)]
    (update-tribe-population tribe [deltaFromChildren deltaFromYoungPopulation deltaFromOldPopulation deltaFromBirths])))

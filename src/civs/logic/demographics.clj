(ns
  ^{:author ftomassetti}
  civs.logic.demographics
  (:require
    [civs.model.core :refer :all]
    [civs.model.society :refer :all]
    [civs.logic.basic :refer :all]
    [civs.logic.globals :refer :all])
  (:import [civs.model.core Population Tribe]))

(import '(com.github.lands Biome))

(require '[civs.model.core :as model])

(defn random-pos-avoiding [world biomes-to-avoid]
  (let [pos (random-pos (.getDimension world))
        biome (biome-at world pos)]
    (if (in? biomes-to-avoid biome)
      (random-pos-avoiding world biomes-to-avoid)
      pos)))

(defn- random-initial-population []
  (model/Population. (crand-int 15) (crand-int 15) (crand-int 15) (crand-int 5) (crand-int 5)))

(def unhospital-biomes #{com.github.lands.Biome/OCEAN com.github.lands.Biome/GLACIER com.github.lands.Biome/ICELAND})

(defn generate-tribe
  "Return a map game, tribe"
  [game]
  (let [world (.world game)]
    (create-tribe game :unnamed (random-pos-avoiding world unhospital-biomes) (random-initial-population) initial-culture initial-society)))

(defn- base-prosperity-per-activity-in-biome
  "Do not consider population limits"
  [biome activity]
  (case activity
    :gathering-and-hunting (case (.name biome)
                           "OCEAN" (throw (Exception. (str "No prosperity in the ocean")))
                           "ICELAND"     0.5
                           "TUNDRA"      0.80
                           "ALPINE"      0.80
                           "GLACIER"     0.5
                           "GRASSLAND"   0.835
                           "ROCK_DESERT" 0.775
                           "SAND_DESERT" 0.775
                           "FOREST"      0.820
                           "SAVANNA"     0.805
                           "JUNGLE"      0.825
                           (throw (Exception. (str "Unknown biome" biome))))
    :agriculture (case (.name biome)
                   "OCEAN" (throw (Exception. (str "No prosperity in the ocean")))
                   "ICELAND"     0.5
                   "TUNDRA"      0.80
                   "ALPINE"      0.80
                   "GLACIER"     0.5
                   "GRASSLAND"   0.835
                   "ROCK_DESERT" 0.65
                   "SAND_DESERT" 0.65
                   "FOREST"      0.78
                   "SAVANNA"     0.800
                   "JUNGLE"      0.785
                   (throw (Exception. (str "Unknown biome" biome))))
  (throw Exception (str "Unknown activity " activity))))

(defn- prosperity-temperature-multiplier-with-range [negative-limit neutral-value positive-limit temperature]
  (let [direction (if (> positive-limit negative-limit) :asc :desc)
        mod (case direction
                  :asc (if (< temperature neutral-value)
                         (let [diff       (- neutral-value temperature)
                               max-diff   (- neutral-value negative-limit)
                               negativity (/ diff max-diff)]
                           (* negativity -0.2))
                         (let [diff       (- temperature neutral-value )
                               max-diff   (- positive-limit neutral-value )
                               positivity (/ diff max-diff)]
                           (* positivity 0.2)))
                  :desc (if (< temperature neutral-value)
                          (let [diff       (- neutral-value temperature)
                                max-diff   (- neutral-value positive-limit)
                                positivity (/ diff max-diff)]
                            (* positivity 0.2))
                          (let [diff       (- temperature neutral-value )
                                max-diff   (- negative-limit neutral-value )
                                negativity (/ diff max-diff)]
                            (* negativity -0.2)))
                  nil)]
    (+ 1.0 (/ mod 10.0))))

(defn- prosperity-humidity-multiplier-with-range
  ([negative-limit positive-limit value]
    (prosperity-humidity-multiplier-with-range negative-limit (mean negative-limit positive-limit) positive-limit value))
  ([negative-limit neutral-value positive-limit value]
    (let [ direction (if (> positive-limit negative-limit) :asc :desc)
      mod (case direction
            :asc (if (< value neutral-value)
                   (let [diff       (- neutral-value value)
                         max-diff   (- neutral-value negative-limit)
                         negativity (/ diff max-diff)]
                     (* negativity -0.2))
                   (let [diff       (- value neutral-value )
                         max-diff   (- positive-limit neutral-value )
                         positivity (/ diff max-diff)]
                     (* positivity 0.2)))
            :desc (if (< value neutral-value)
                    (let [diff       (- neutral-value value)
                          max-diff   (- neutral-value positive-limit)
                          positivity (/ diff max-diff)]
                      (* positivity 0.2))
                    (let [diff       (- value neutral-value )
                          max-diff   (- negative-limit neutral-value )
                          negativity (/ diff max-diff)]
                      (* negativity -0.2)))
            nil)]
      (+ 1.0 (/ mod 10.0)))))

(defn- prosperity-temperature-multiplier
  "A factor depending on the temperature"
  [world biome temperature]
  (let [ temperature (if (< temperature 0.0) 0.0 temperature) ; glaciers could be below...
         res (let [low (.get (-> world .getTemperature .thresholds) com.github.lands.Thresholds$Level/LOW)
         med (.get (-> world .getTemperature .thresholds) com.github.lands.Thresholds$Level/MEDIUM)
         high (.get (-> world .getTemperature .thresholds) com.github.lands.Thresholds$Level/HIGH)]
      (case (.name biome)
        "OCEAN"       (throw (Exception. (str "No prosperity in the ocean")))
        "ICELAND"     (prosperity-temperature-multiplier-with-range 0.0  (mean 0.0 low)   low  temperature)
        "TUNDRA"      (prosperity-temperature-multiplier-with-range low          (mean low med)           med  temperature)
        "ALPINE"      (prosperity-temperature-multiplier-with-range 0.0  (mean 0.0 low)   low  temperature)
        "GLACIER"     (prosperity-temperature-multiplier-with-range 0.0  (mean 0.0 low)   low  temperature)
        "GRASSLAND"   (prosperity-temperature-multiplier-with-range med          (mean med high)          high temperature)
        "ROCK_DESERT" (prosperity-temperature-multiplier-with-range med          (mean med high)          high temperature)
        "SAND_DESERT" (prosperity-temperature-multiplier-with-range (* high 3.0) (mean high (* high 3.0)) high temperature)
        "FOREST"      (prosperity-temperature-multiplier-with-range med          (mean med high)          high temperature)
        "SAVANNA"     (prosperity-temperature-multiplier-with-range (* high 3.0) (mean high (* high 3.0)) high temperature)
        "JUNGLE"      (prosperity-temperature-multiplier-with-range (* high 3.0) (mean high (* high 3.0)) high temperature)
        (throw (Exception. (str "Unknown biome" biome)))))]
    (check-in-range res 0.98 1.02 (str "Prosperity temperature modifier for " biome " at " temperature " : " res))
    res))

(defn- prosperity-humidity-multiplier
  "A factor depending on the humidity"
  [world biome humidity]
  (let [ res (let [bottom -0.50
                   q75 (.get (-> world .getHumidity .quantiles) 75)
                   q66 (.get (-> world .getHumidity .quantiles) 66)
                   q50 (.get (-> world .getHumidity .quantiles) 50)
                   q33 (.get (-> world .getHumidity .quantiles) 33)
                   q10 (.get (-> world .getHumidity .quantiles) 10)
                   top (* 3.0 q10)]
               (case (.name biome)
                 "OCEAN"       (throw (Exception. (str "No prosperity in the ocean")))
                 "ICELAND"     1.0
                 "TUNDRA"      1.0
                 "ALPINE"      1.0
                 "GLACIER"     1.0
                 "GRASSLAND"   (prosperity-humidity-multiplier-with-range q66    q33 humidity)
                 "ROCK_DESERT" (prosperity-humidity-multiplier-with-range bottom q66 humidity)
                 "SAND_DESERT" (prosperity-humidity-multiplier-with-range bottom q50 humidity)
                 "FOREST"      (prosperity-humidity-multiplier-with-range top    q33 humidity)
                 "SAVANNA"     (prosperity-humidity-multiplier-with-range q50 (mean q50 q10) top humidity)
                 "JUNGLE"      (prosperity-humidity-multiplier-with-range top    q33 humidity)
                 (throw (Exception. (str "Unknown biome" biome)))))]
    (check-in-range res 0.975 1.025 (str "Prosperity humidity modifier for " biome " at " humidity " : " res))
    res))

(defn prosperity-temperature-multiplier-at [world pos]
  (let [biome (biome-at world pos)
        temperature (temperature-at world pos)]
    (try
      (prosperity-temperature-multiplier world biome temperature)
      (catch Exception e
        (throw
          (Exception.
            (str "Problem while calculating prosperity temperature multiplier at " pos
              " where biome should be " biome
              ". World " (.getName world)) e))))))

(defn prosperity-humidity-multiplier-at [world pos]
  (let [biome (biome-at world pos)
        humidity (humidity-at world pos)]
    (try
      (prosperity-humidity-multiplier world biome humidity)
      (catch Exception e
        (throw
          (Exception.
            (str "Problem while calculating prosperity humidity multiplier at " pos
              " where biome should be " biome
              ". World " (.getName world)) e))))))

(defn base-prosperity-per-activity [world pos activity]
  (*
    (base-prosperity-per-activity-in-biome (biome-at world pos) activity)
    (prosperity-temperature-multiplier-at world pos)
    (prosperity-humidity-multiplier-at world pos)))

(defn crowding-per-activity [group activity]
  (let [ actives (-> group .population active-persons)
         tot     (-> group .population total-persons)
         max-supportable (case activity
                           :gathering-and-hunting 110
                           :agriculture           1000
                           (throw Exception (str "Unknown activity " activity)))
         pop-supportable (* actives (case activity
                                      :gathering-and-hunting 3.5
                                      :agriculture           6.5
                                      (throw Exception (str "Unknown activity " activity))))
         pop-supportable (min max-supportable pop-supportable)]
    (if (< tot pop-supportable)
      1.0
      (if (or (= pop-supportable 0.0) (= tot 0))
        0.0
        (/ 1.0 (/ tot pop-supportable))))))

(defn prosperity-in-pos-per-activity [game tribe pos activity]
  (let [world    (.world game)
        base     (base-prosperity-per-activity world pos activity)
        crowding (crowding-per-activity tribe activity)]
    (saturate (* base crowding) 1.0)))

(defn known-activities [group]
  (if (know? group :agriculture)
    [:gathering-and-hunting :agriculture ]
    [:gathering-and-hunting]))

(defn chosen-activity
  [game group pos]
  (let [prosperity-by-activity (map
                                 (fn [activity] {:activity activity :prosperity (prosperity-in-pos-per-activity game group pos activity)})
                                 (known-activities group))]
    (:activity (first (sort-by :prosperity prosperity-by-activity)))))

(defn crowding
  "The current crowding of the group at this time"
  [game group]
  (let [activity (chosen-activity game group (.position group))]
    (crowding-per-activity group activity)))

(defn prosperity-in-pos
  "The prosperity a tribe would have in a given position"
  [game group pos]
  (let [world (.world game)]
    (apply max
      (map
        (fn [activity]
          (prosperity-in-pos-per-activity game group pos activity))
        (known-activities group)))))

(defn prosperity
  "A number in [0,1] whic indicates how well the tribe is living.
  For now it just depends on the kind of biome where the tribe is.
  Increase for young men and women, reduce for children and old people
  Depending on the kind of activity done (gathering/agriculture)
  certain number of people can be supported"
  [game group]
  (prosperity-in-pos game group (.position group)))

(defn men-availability-factor [young-men young-women]
  ; check necessary to avoid division by zero
  (if (> young-women 0)
    (let [men-factor (/ (float young-men) young-women)
          res (/ (+ men-factor 0.5) 2)]
      (saturate (saturate res 1.0) (* men-factor 3)))
    0))

(defn update-births
  "This method returns a delta"
  [world tribe prosperity]
  (let [young-men               (-> tribe :population :young-men)
        young-women             (-> tribe :population :young-women)
        men-availability-factor (men-availability-factor young-men young-women)
        women-fertility         (* young-women 1.1 (perturbate-high prosperity))
        births                  (round (* women-fertility men-availability-factor))
        ; In primitive nomadic societies mothers tend to have one child every four
        ; years
        births                  (if (nomadic? tribe) (* 0.75 births) births)]
    (when (> births 0)
      (fact :births {:tribe (.id tribe) :n births}))
    (Population. births 0 0 0 0)))

(defn update-children
  "Children can die or grow in to young men or women.
  This method returns a delta"
  [world tribe prosperity]
  (let [mortality (* (opposite prosperity) 0.35)
        n-children (-> tribe :population :children)
        [dead, grown] (rsplit-by n-children mortality)
        [men, women] (rsplit-by grown 0.5)]
    (when (> dead 0)
      (fact :children-dead {:tribe (.id tribe) :n dead}))
    (when (> men 0)
      (fact :children-grown-as-men {:tribe (.id tribe) :n men}))
    (when (> women 0)
      (fact :children-grown-as-women {:tribe (.id tribe) :n women}))
    (Population. (* -1 n-children) men women 0 0)))

(defn update-young-population
  "Young men and women can die, remain young or grow old.
  This method returns a delta"
  [world tribe prosperity]
  (let [mortality-men     (* (opposite prosperity) 0.22)
        mortality-women   (* (opposite prosperity) 0.22)
        n-young-men       (-> tribe :population :young-men)
        n-young-women     (-> tribe :population :young-women)
        [dead-m, alive-m] (rsplit-by n-young-men mortality-men)
        [dead-w, alive-w] (rsplit-by n-young-women mortality-women)
        [grown-m, _]      (rsplit-by alive-m 0.25)
        [grown-w, _]      (rsplit-by alive-w 0.25)]
    (when (> dead-m 0)
      (fact :young-men-dead {:tribe (.id tribe) :n dead-m}))
    (when (> dead-w 0)
      (fact :young-women-dead {:tribe (.id tribe) :n dead-w}))
    (when (> grown-m 0)
      (fact :young-men-grew-old {:tribe (.id tribe) :n grown-m}))
    (when (> grown-w 0)
      (fact :young-women-grew-old {:tribe (.id tribe) :n grown-w}))
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
    (when (> dead-m 0)
      (fact :old-men-dead {:tribe (.id tribe) :n dead-m}))
     (when (> dead-w 0)
      (fact :old-women-dead {:tribe (.id tribe) :n dead-w}))
    (Population. 0 0 0 (* -1 dead-m) (* -1 dead-w))))


(defn- sum-population [pop delta]
  (let [new-children    (+ (.children pop)    (.children delta))
        new-young-men   (+ (.young-men pop)   (.young-men delta))
        new-young-women (+ (.young-women pop) (.young-women delta))
        new-old-men     (+ (.old-men pop)     (.old-men delta))
        new-old-women   (+ (.old-women pop)   (.old-women delta))]
    (Population. new-children new-young-men new-young-women new-old-men new-old-women)))

(defn update-tribe-population
  "this sum the various deltas to the tribe population"
  [tribe deltas]
  (let [new-population (reduce sum-population (.population tribe) deltas)]
    (assoc tribe :population new-population)))

(defn update-population
  "We use deltas, so that we first calculated all the changes on the original
   population and only then we apply all the changes, after all the calculations
   are done, otherwise the first calculations would influence the results of the
   other steps"
  [game tribe]
  (let [p (prosperity game tribe)
        world (.world game)
        deltaFromChildren        (update-children         world tribe p)
        deltaFromYoungPopulation (update-young-population world tribe p)
        deltaFromOldPopulation   (update-old-population   world tribe p)
        deltaFromBirths          (update-births world tribe p)]
    (update-tribe-population tribe [deltaFromChildren deltaFromYoungPopulation deltaFromOldPopulation deltaFromBirths])))

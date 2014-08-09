(ns
  ^{:author ftomassetti}
  civs.logic.demographics
  (:require
    [civs.model :refer :all]
    [civs.logic :refer :all])
  (:import [civs.model Population Tribe]))

(defn men-availability-factor [young-men young-women]
  (let [ men-factor (/ (float young-men) young-women)
         res (/ (+ men-factor 0.5) 2)]
    (saturate (saturate res 1.0) (* men-factor 3))))

(defn update-births
  "This method returns a delta"
  [world tribe prosperity]
  (let [young-men (-> tribe :population :young-men)
        young-women (-> tribe :population :young-women)
        men-availability-factor (men-availability-factor young-men young-women)
        women-fertility (* young-women (perturbate-high prosperity))
        births (round (* women-fertility men-availability-factor))]
    (fact :births {:tribe tribe :n births} (str births " children were born"))
    (Population. births 0 0 0 0)))

(defn update-children
  "Children can die or grow in to young men or women.
  This method returns a delta"
  [world tribe prosperity]
  (let [mortality (perturbate-med (opposite prosperity))
        n-children (-> tribe :population :children)
        [dead, grown] (split-by n-children mortality)
        [men, women] (split-by grown (perturbate-low 0.5))]
    (fact :children-dead {:tribe tribe :n dead} (str dead " children died"))
    (fact :children-grown-as-men {:tribe tribe :n men} (str men " children grew as men"))
    (fact :children-grown-as-women {:tribe tribe :n women} (str women " children grew as women"))
    (Population. (* -1 n-children) men women 0 0)))

(defn update-young-population
  "Young men and women can die, remain young or grow old.
  This method returns a delta"
  [world tribe prosperity]
  (let [mortality-men     (mean 0.0 (opposite prosperity))
        mortality-women   (mean 0.0 (opposite prosperity))
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
  (let [mortality-men   (mean 0.7 (opposite prosperity))
        mortality-women (mean 0.7 (opposite prosperity))
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

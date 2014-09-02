(ns
  ^{:author ftomassetti}
  civs.logic.basic
  (:import [civs.model.core Population Tribe])
  (:require
    [civs.model.core :refer :all]))

(import '(java.util Random))

(require '[civs.model.core :as model])

(defn crand-int
  "c stands for c, so to not override clojure.core/rand-int"
  [n]
  (.nextInt r n))

(defn crand-float
  "c stands for c, so to not override clojure.core/rand-float"
  []
  (.nextFloat r))

(defn roll [prob]
  (< (crand-float) prob))

(defn check-in-range [n min max & msg]
  (let [msg (if (nil? msg) "" msg)]
    (when (< n min)
      (throw (Exception. (str "Too low" msg)))
    (when (> n max)
      (throw (Exception. (str "Too high: limit " max " value " n msg)))))))

(defn force-in-range [n min max]
  (when (< n min)
    min)
  (when (> n max)
    max)
  n)

(defn opposite [n]
  (check-in-range n 0.0 1.0)
  (- 1.0 n))

(defn perturbate
  "n should be in [0,1]"
  [n perturbaction-factor]
  (check-in-range n 0.0 1.0)
  (let [perturbation (* (- (crand-float) 0.5) perturbaction-factor)
        res (+ n perturbation)
        res (force-in-range res 0 1.0)]
    res))

(defn perturbate-high
  "n should be in [0,1]"
  [n]
  (perturbate n 1.0))

(defn perturbate-med
  "n should be in [0,1]"
  [n]
  (perturbate n 0.33))

(defn perturbate-low
  "n should be in [0,1]"
  [n]
  (perturbate n 0.2))

(defn rand-range [a b]
  (when (> a b)
    (throw (Exception. "Invalid range")))
  (let [diff (- b a)]
    (+ a (crand-int diff))))

(defn random-pos [dimension]
  (let [x (.nextInt r (.getWidth dimension))
        y (.nextInt r (.getHeight dimension))]
    {:x x :y y}))

(defn round [v]
  (int (+ 0.5 v)))

(defn mean [a b]
  (/ (+ a b) 2.0))

(defn split-by
  "factor is in [0,1]"
  [n factor]
  (check-in-range factor 0.0 1.0)
  (let [a (round (* n factor))
        b (- n a)]
    [a b]))

(defn rsplit-by
  "factor is in [0,1]"
  [n factor]
  (check-in-range factor 0.0 1.0)
  (let
    [values (repeatedly n #(if (< (crand-float) factor) [1 0] [0 1]))]
    (reduce #(map + %1 %2) [0 0] values)))

(defn saturate [value max]
  (if (> value max)
    max
    value))

; ======================================
; Globals
; ======================================

(def current-turn nil)

(defn get-current-turn []
  current-turn)

(defn inc-current-turn []
  (def current-turn (inc (get-current-turn))))

(defn reset-current-turn []
  (def current-turn 0))


(def facts (atom []))

(defn fact [type params]
  (swap! facts conj (assoc params :type type)))

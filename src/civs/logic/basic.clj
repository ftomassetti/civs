(ns
  ^{:author ftomassetti}
  civs.logic.basic
  (:import [civs.model.core Population Group])
  (:require
    [civs.model.core :refer :all]))

(import '(java.util Random))

(require '[civs.model.core :as model])

(def __internal_random__ (Random. 1))

(defn crand-int
  "c stands for c, so to not override clojure.core/rand-int"
  [n]
  (.nextInt __internal_random__ n))

(defn crand-float
  "c stands for c, so to not override clojure.core/rand-float"
  []
  (.nextFloat __internal_random__))

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
  (let [x (crand-int (.getWidth dimension))
        y (crand-int (.getHeight dimension))]
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

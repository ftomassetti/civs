(ns civs.logic.basic-test
  (:require [clojure.test :refer :all]
            [civs.core :refer :all]
            [civs.model.core :refer :all]
            [civs.logic.core :refer :all]
            [civs.logic.basic :refer :all]
            [civs.logic.demographics :refer :all]
            [civs.logic.tribe-choices :refer :all]))

(deftest test-roll
  (with-redefs [crand-float (constantly 0.1)]
    (= false (roll 0.0))
    (= true (roll 0.1))
    (= true (roll 0.2))))

(deftest test-opposite
  (= 0.5 (opposite 0.5))
  (= 0.0 (opposite 1.0))
  (= 1.0 (opposite 0.0)))

(deftest test-force-in-range
  (= 0.5 (force-in-range 0.5 0 1.0))
  (= 0.0 (force-in-range -0.1 0 1.0))
  (= 1.0 (force-in-range 1.1 0 1.0)))

(deftest test-perturbate-with-null-perturbation
  (with-redefs [crand-float (constantly 1.0)]
    (= 0.7 (perturbate 0.7 0))))

(deftest test-perturbate-with-null-rand-value
  (with-redefs [crand-float (constantly 0.0)]
    (= 0.7 (perturbate 0.7 10.0))))

(deftest test-perturbate-with-low-rand-value
  ; Rolling a value lower than 0.5 should produce a result lower the original value
  (with-redefs [crand-float (constantly 0.3)]
    (< (perturbate 0.6 3.0) 0.6)))

(deftest test-perturbate-with-high-rand-value
  ; Rolling a value greater than 0.5 should produce a result higher the original value
  (with-redefs [crand-float (constantly 0.7)]
    (> (perturbate 0.6 3.0) 0.6)))

(deftest test-perturbate-an-higher-factor-should-have-a-stronger-effect
  ; The perturbation will be positive (crand-float return number higher than 0.5)
  ; Result should higher when the factor is higher
  (with-redefs [crand-float (constantly 0.7)]
    (> (perturbate 0.6 3.0) (perturbate 0.6 2.0)))
  ; The perturbation will be negative (crand-float return number lower than 0.5)
  ; Result should lower when the factor is higher
  (with-redefs [crand-float (constantly 0.4)]
    (> (perturbate 0.6 2.0) (perturbate 0.6 3.0))))

(deftest test-perturbate-low-med-high
  ; The perturbation will be positive (crand-float return number higher than 0.5)
  ; Result should higher when the factor is higher
  (with-redefs [crand-float (constantly 0.7)]
    (> (perturbate-high 0.6) (perturbate-med 0.6))
    (> (perturbate-med 0.6) (perturbate-low 0.6)))
  ; The perturbation will be negative (crand-float return number lower than 0.5)
  ; Result should lower when the factor is higher
  (with-redefs [crand-float (constantly 0.4)]
    (> (perturbate-low 0.6) (perturbate-med 0.6))
    (> (perturbate-med 0.6) (perturbate-high 0.6))))

(deftest test-mean
  (= 0.7 (mean 0.5 0.9))
  (= 0.0 (mean 0.0 0.0))
  (= 0.0 (mean -0.5 0.5)))

(deftest test-saturate
  (= 0.6 (saturate 0.6 1.0))
  (= 1.0 (saturate 1.6 1.0)))

(deftest test-rand-range
  (with-redefs [crand-int (constantly 2)]
    (= 5 (rand-range 3 10)))
  (with-redefs [crand-int (constantly 0)]
    (= 3 (rand-range 3 10))))
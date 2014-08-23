(ns
  ^{:author ftomassetti}
  civs.model.core
  (:require
    [civs.model.basic :refer :all]
    [potemkin :refer :all])
  (:import
    [civs.model.basic Tribe Population Town]))

(import-vars
  [civs.model.basic
   Tribe Population Town])
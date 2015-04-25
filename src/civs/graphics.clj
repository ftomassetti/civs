(ns
  ^{:author ftomassetti}
  civs.graphics
  (:require [civs.model.core :refer :all]
            [civs.logic.basic :refer :all])
  (import java.io.File)
  (import java.awt.Color)
  (import java.awt.image.BufferedImage)
  (import javax.imageio.ImageIO))

(defn- population-map-color [game pos]
  (let [w (.world game)
        ocean-color (Color. 0 0 255)]
    (if (isLand w pos)
      (let [v (game-total-pop-in-pos game pos)
            v (saturate v 255)]
        (Color. 255 (- 255 v) (- 255 v)))
      ocean-color)))

(defn draw-population-map
  [game filename]
  (let [width  (game-width  game)
        height (game-height game)
        bi (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        gr (.createGraphics bi)]
    (doall (for [x (doall (range width)) y (doall (range height))]
      (do
        (.setColor gr (population-map-color game {:x x :y y}))
        (.fillRect gr x y (inc x) (inc y)))))
    (ImageIO/write bi "png" (File. filename))))

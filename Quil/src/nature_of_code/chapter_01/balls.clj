(ns nature-of-code.chapter-01.balls
  (:require [quil.core :refer [defsketch background smooth ellipse stroke
                               fill frame-rate stroke-weight]]
            [nature-of-code.vectors :as v]))






; The vectored version is much nicer.
(def location (ref nil))
(def velocity (ref nil))

(defn setup-vectored []
  (setup)
  (dosync
    (ref-set location (v/vec 100 100))
    (ref-set velocity (v/vec 2.5 5))))

(defn draw-vectored []
  (background 255)
  (dosync
    (alter location v/+ @velocity)
    (alter velocity update-in [:x] bound-flip (:x @location) screen-width)
    (alter velocity update-in [:y] bound-flip (:y @location) screen-height))
  (let [[x y] @location]
    (ellipse x y 20 20)))

(comment
  (defsketch s-vectored
    :title "Vectored Ball"
    :setup setup-vectored
    :draw draw-vectored
    :size [screen-width screen-height]))

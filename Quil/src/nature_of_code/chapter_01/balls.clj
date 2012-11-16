(ns nature-of-code.chapter-01.balls
  (:require [quil.core :refer [defsketch background smooth ellipse stroke
                               fill frame-rate stroke-weight]]
            [nature-of-code.vectors :as v]))


(def screen-width 640)
(def screen-height 200)

(defn setup []
  (stroke 0)
  (fill 175)
  (stroke-weight 2)
  (frame-rate 40)
  (background 255)
  (smooth))

; For the vectorless example we have four pieces of state.
(def x (ref nil))
(def y (ref nil))
(def x-speed (ref nil))
(def y-speed (ref nil))

(defn setup-vectorless []
  (setup)
  (dosync
    (ref-set x 100)
    (ref-set y 100)
    (ref-set x-speed 1)
    (ref-set y-speed 3.3)))

(defn bound-flip
  "Negate v if x is lower than zero or greater than the max."
  [v x max-x]
  (if (or (neg? x) (> x max-x))
    (- v)
    v))

(defn draw-vectorless []
  (background 255)
  (dosync
    (alter x + @x-speed)
    (alter y + @y-speed)
    (alter x-speed bound-flip @x screen-width)
    (alter y-speed bound-flip @y screen-height))
  (ellipse @x @y 30 30))

(comment
  (defsketch s-vectorless
    :title "Vectorless Ball"
    :setup setup-vectorless
    :draw draw-vectorless
    :size [screen-width screen-height]))

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

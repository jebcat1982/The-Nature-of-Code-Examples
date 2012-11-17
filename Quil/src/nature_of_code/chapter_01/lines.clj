(ns nature-of-code.chapter-01.lines
  (:require [quil.core :refer [defsketch background smooth stroke line fill rect
                               stroke-weight mouse-x mouse-y translate]]
            [nature-of-code.vectors :as v]))



(defn draw-sub []
  (background 255)
  (let [mouse (v/vec (mouse-x) (mouse-y))
        center (v/vec (/ screen-width 2)
                      (/ screen-height 2))
        result (v/- mouse center)]
    (translate (/ screen-width 2) (/ screen-height 2))
    (line 0 0 (:x result) (:y result))))

(defn draw-mult []
  (background 255)
  (let [mouse (v/vec (mouse-x) (mouse-y))
        center (v/vec (/ screen-width 2)
                      (/ screen-height 2))
        result (v/* (v/- mouse center) 0.5)]
    (translate (/ screen-width 2) (/ screen-height 2))
    (line 0 0 (:x result) (:y result))))

(comment
  (defsketch s-mult
    :title "Multiplication of Vectors"
    :draw draw-mult
    :setup setup
    :size [screen-width screen-height]))

(defn draw-mag []
  (background 255)
  (let [mouse (v/vec (mouse-x) (mouse-y))
        center (v/vec (/ screen-width 2)
                      (/ screen-height 2))
        result (v/- mouse center)
        size (v/magnitude result)]
    (fill 0)
    (rect 0 0 size 10)
    (translate (/ screen-width 2) (/ screen-height 2))
    (line 0 0 (:x result) (:y result))))

(comment
  (defsketch s-mag
    :title "Magnitude of Vectors"
    :draw draw-mag
    :setup setup
    :size [screen-width screen-height]))

(defn draw-norm []
  (background 255)
  (let [mouse (v/vec (mouse-x) (mouse-y))
        center (v/vec (/ screen-width 2)
                      (/ screen-height 2))
        result (v/* (v/normalize (v/- mouse center)) 50)
        size (v/magnitude result)]
    (translate (/ screen-width 2) (/ screen-height 2))
    (line 0 0 (:x result) (:y result))))

(comment
  (defsketch s-norm
    :title "Normalized Vectors"
    :draw draw-norm
    :setup setup
    :size [screen-width screen-height]))

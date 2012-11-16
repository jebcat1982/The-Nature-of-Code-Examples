(ns nature-of-code.chapter-01.walkers
  (:require [roul.random :as r]
            [quil.core :refer [defsketch stroke point background frame-rate]]
            [nature-of-code.vectors :as v]
            [nature-of-code.utils :refer [defrec Renderable render]]))

; Most of this is just bookkeeping copied from the introduction.
(def screen-width 640)
(def screen-height 360)

(defrecord Walker [location])

(defn make-walker []
  (->Walker (v/vec (/ screen-width 2) (/ screen-height 2))))

(extend-type Walker
  Renderable
  (render [{[x y] :location}] ; Need to pull x and y out of the vector here.
    (stroke 0)
    (point x y)))

(def w (ref nil))

(defn setup []
  (frame-rate 30)
  (dosync (ref-set w (make-walker)))
  (background 255))

(defn draw [step-fn]
  (dosync (alter w step-fn))
  (render @w))

; Here's the new part.  Instead of handling x and y separately, we'll use
; vectors as the location and step.
(defn step-vectored [walker]
  (let [step (v/vec (r/rand-gaussian 0 2)
                    (r/rand-gaussian 0 2))]
    (->Walker (v/+ (:location walker) step))))

(comment
  (defsketch s-vectored
    :title "Vectored Walker"
    :setup setup
    :draw (partial draw step-vectored)
    :size [screen-width screen-height]))

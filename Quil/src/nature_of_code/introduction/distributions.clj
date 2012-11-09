(ns nature-of-code.introduction.distributions
  (:require [roul.random :as r]
            [quil.core :refer [defsketch stroke point background fill rect
                               no-stroke ellipse smooth]]))


; Again we'll hack around Quil's broken (width) and (height) and broken :size.
(def screen-size [640 240])
(def screen-width 640)
(def screen-height 240)

; And we'll use a ref to track state.
(def random-counts (ref []))

; We'll initialize ref to a vector instead of a sequence, because we'll need to
; index into it and modify it on every tick.
(defn setup []
  (dosync (ref-set random-counts (vec (repeat 20 0)))))

; On each tick, we update a random bucket, and then map over all the buckets to
; draw them.  Pretty simple.
(defn draw []
  (background 255)
  (let [bars (count @random-counts)]
    (dosync (alter random-counts update-in [(int (r/rand bars))] inc))
    (stroke 0)
    (fill 175)
    (let [bar-width (/ screen-width bars)
          draw-bar (fn [b n]
                     (let [bx (* b bar-width)
                           by (- screen-height n)
                           bw (dec bar-width)
                           bh n]
                       (rect bx by bw bh)))]
      (dorun (map-indexed draw-bar @random-counts)))))

(comment
  (defsketch s-uniform
    :title "Distribution of (rand)"
    :setup setup
    :draw draw
    :size [screen-width screen-height]))

; The Gaussian stuff is in utils because it's useful.  We'll draw the circles
; here.
(defn setup-gaussian-circles []
  (background 255))

(defn draw-gaussian-circles []
  (no-stroke)
  (fill 0 10)
  (ellipse (r/rand-gaussian 320 60) (/ screen-height 2) 16 16))

(comment
  (defsketch s-gaussian
    :title "Gaussian Distribution"
    :setup setup-gaussian-circles
    :draw draw-gaussian-circles
    :size [screen-width screen-height]))

(defn setup-splatter []
  (background 255)
  (no-stroke)
  (smooth))

(defn draw-splatter []
  (fill 0 10)
  (let [x (r/rand-gaussian (/ screen-width 2)
                           (/ screen-width 7))
        y (r/rand-gaussian (/ screen-height 2)
                           (/ screen-height 7))
        r (r/rand-gaussian 5 2)
        sat (r/rand-gaussian 120 40 0 255)]
    (fill sat 0 0)
    (ellipse x y r r)))

(def screen-size [640 480])
(def screen-width 640)
(def screen-height 480)

(comment
  (defsketch s-splatter
    :title "Paint Splatter"
    :setup setup-splatter 
    :draw draw-splatter
    :size [screen-width screen-height]))

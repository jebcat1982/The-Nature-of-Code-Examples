(ns nature-of-code.introduction.noise
  (:require [quil.core :refer [defsketch noise background smooth fill stroke
                               map-range ellipse frame-rate set-pixel
                               update-pixels load-pixels color pixels
                               noise-detail]]
            [quil.helpers.seqs :refer [perlin-noise-seq]]))


(def screen-width 640)
(def screen-height 400)

; First we'll mimic the example code from the book.
(def t (ref 0))

(defn setup-noise-ball []
  (frame-rate 60)
  (smooth)
  (fill 200)
  (stroke 0))

(defn draw-noise-ball []
  (background 255)
  (let [n (noise @t)
        x (map-range n 0 1 0 screen-width)
        y (/ screen-height 2)]
    (ellipse x y 20 20))
  (dosync (alter t #(+ 0.005 %))))

(comment
  (defsketch s-noise-ball
    :title "Perlin Noise Ball"
    :setup setup-noise-ball
    :draw draw-noise-ball
    :size [screen-width screen-height]))

; Next let's use perlin-noise-seq to create an infinite sequence of noise
; (pre-scaled too!) we can pull from.  We just need to be careful to not hold on
; to the head or the items will never be GC'ed.
(def noise-seq (ref (map #(map-range % 0 1 0 screen-width)
                         (perlin-noise-seq (rand) 0.005))))

(defn draw-noise-ball-2 []
  (background 255)
  (let [[x & tail] @noise-seq
        y (/ screen-height 2)]
    (ellipse x y 20 20)
    (dosync (ref-set noise-seq tail))))

; If you want to see the effects of retaining a head reference (i.e.: Clojure
; eating your RAM) uncomment this.  It'll take a while though, because each item
; in the seq is just a float, so it's not terribly memory intensive.
; (def head @noise-seq)

(comment
  (defsketch s-noise-ball-2
    :title "Perlin Noise Ball 2"
    :setup setup-noise-ball
    :draw draw-noise-ball-2
    :size [screen-width screen-height]))


; Now for the 2D noise drawing.
(defn setup []
  (noise-detail 8 0.45)
  (frame-rate 15)
  (background 255))

(def t (ref 0))

; And now we're at the point where Clojure gets ugly.  Type hinting ahoy!
(defn draw-noise-2d []
  (dosync (alter t #(+ % 0.05)))
  (let [^ints px (pixels)
        t @t]
    (doseq [x (range screen-width)
            y (range screen-height)
            :let [xo (* 0.010 x)
                  yo (* 0.010 y)
                  pi (int (+ x (* screen-width y)))
                  b (int (color (map-range (noise xo yo t)
                                           0 1
                                           0 255)))]]
      (aset px pi b)))
  (update-pixels))


(comment
  (defsketch s-noise-2d
    :title "Perlin Noise 2D"
    :setup setup
    :draw draw-noise-2d
    :renderer :p2d
    :size [screen-width screen-height]))

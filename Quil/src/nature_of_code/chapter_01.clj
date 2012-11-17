(ns nature-of-code.chapter-01
  (:require [quil.core :refer [defsketch background smooth ellipse stroke
                               fill frame-rate stroke-weight point translate
                               line mouse-x mouse-y rect]]
            [roul.random :as r]
            [nature-of-code.utils :refer [Renderable render Tickable tick
                                          defrec]]
            [nature-of-code.vectors :as v]))


; Once again we define these constants in one place.
(def screen-width 640)
(def screen-height 400)

; All of the balls will be records that implement Renderable and Tickable, just
; like the examples from the Introduction.  We'll keep the ball in a ref.
(def b (ref nil))

; We're going to start off with a simple, non-vectored ball example.  The base
; setup is going to be the same for a lot of these examples, so we'll pull it
; out into a helper function just like last chapter.
(defn setup-ball [fresh-ball]
  (stroke 0)
  (fill 175)
  (stroke-weight 2)
  (background 255)
  (smooth)
  (dosync (ref-set b fresh-ball)))

; And a helper function for drawing all kinds of balls.
(defn render-ball [x y]
  (ellipse x y 20 20))

; Now we'll implementing the VectorlessBall.
(defrec VectorlessBall [x y x-speed y-speed])

; Vectorless balls are rendered normally.
(extend-type VectorlessBall Renderable
  (render [[x y]]
    (render-ball x y)))

; Instead of manually doing this all over, we'll have a little helper function
; for flipping velocities when they overstep the bounds.
(defn bound-flip
  "Negate v if n is lower than zero or greater than the max."
  [v n max-n]
  (if (or (neg? n) (> n max-n))
    (- v)
    v))

; But to tick them we have to update all of their guts ourselves.
(extend-type VectorlessBall Tickable
  (tick [{:keys [x y x-speed y-speed] :as this}]
    (let [x (+ x x-speed)
          y (+ y y-speed)]
    (-> this
      (assoc :x x :y y)
      (update-in [:x-speed] bound-flip x screen-width)
      (update-in [:y-speed] bound-flip y screen-height)))))

; The drawing function is going to be the same for all balls (they'll use the
; different protocol implementations though).  We just tick them and then render
; them.
(defn draw-ball []
  (dosync (alter b tick))
  (background 255)
  (render @b))

; First sketch of the chapter!
(comment
  (defsketch s-vectorless
    :title "Vectorless Ball"
    :setup (partial setup-ball
                    (->VectorlessBall 100 100 1 1.3))
    :draw draw-ball
    :size [screen-width screen-height]))

; I've pulled out all the Vector code into its own namespace,
; nature-of-code.vectors, and refer'ed it into this one as v.  So head over to
; that file if you want to see the implementation of Vector functionality.
; There's some cool stuff in it, I think.

; In this file we'll now continue on to the Vectored ball example.  Once again,
; it's a record.
(defrec VectorBall [location velocity])

; All the render-ball helper cares about is the x and y, so we'll pull that out
; for it.
(extend-type VectorBall Renderable
  (render [{[x y] :location}]
    (render-ball x y)))

; Clojure syntax gets horrible from here on if we don't make a vector-aware
; bound-flipper.  Trust me.
;
; At least we'll make it a bit less terrible by making a function that will work
; to flip the velocity components of anything with a velocity and a location.
(defn bounce-bounds [{[x y] :location :as thing}]
  (-> thing
    (update-in [:velocity :x] bound-flip x screen-width)
    (update-in [:velocity :y] bound-flip y screen-height)))

; Ticking is much simpler now.
(extend-type VectorBall Tickable
  (tick [{:keys [velocity] :as this}]
    (-> this
      (update-in [:location] v/+ velocity)
      bounce-bounds)))

; Let's draw it.
(comment
  (defsketch s-vectorball
    :title "Vector Ball"
    :setup (partial setup-ball
                    (->VectorBall (v/vec 100 100)
                                  (v/vec 2 1)))
    :draw draw-ball
    :size [screen-width screen-height]))

; Exercise 1.2: convert a walker to use vectors.  This isn't too bad.  We just
; convert the x and y inside the walker record into a location vector.
(defrec VectorWalker [location])

; This is the only walker we'll be making in this chapter, so no need to create
; helper functions.
(extend-type VectorWalker Renderable
  (render [{[x y] :location}] ; This is really concise thanks to defrec.
    (stroke 0)
    (point x y)))

; To keep with the vector idea, we'll create the step as a randomly-sized
; vector, then add that to the location.
(extend-type VectorWalker Tickable
  (tick [this]
    (let [step (v/vec (r/rand-gaussian 0 2)
                      (r/rand-gaussian 0 2))]
      (update-in this [:location] v/+ step))))

; We'll keep it in a separate ref from the balls.
(def w (ref nil))

; Just because it may come in handy, let's make a vector that points at the
; center of the screen.  We can use it anywhere we like without fear because
; it's immutable.
(def center (v/vec (/ screen-width 2) (/ screen-height 2)))

; Again, this is the only walker so no need to bother with helpers for setup or
; drawing.
(defn setup-walker []
  (dosync (ref-set w (->VectorWalker center)))
  (background 255))

(defn draw-walker []
  (dosync (alter w tick))
  (render @w))

(comment
  (defsketch s-walker
    :title "Vectored Walker"
    :setup setup-walker
    :draw draw-walker
    :size [screen-width screen-height]))

; Exercise 1.3 seems pretty crazy.  Maybe I just haven't done enough Processing
; and it's actually really easy... but I'm going to skip it for now.

; The vector subtraction stuff is in the vectors namespace.

; Now for the series of Line examples.

; First: a simple line that follows the mouse.  We'll use simple helpers for
; setup and draw that will make things easier.
(defn setup-line []
  (smooth)
  (frame-rate 30)
  (stroke 0))

(defn draw-line-from-center
  "Draw a line from the center to the given vector."
  [target-vector]
  ; This uses our handy center vector to avoid typing (/ ... 2) over and over.
  (let [[x y] target-vector
        [cx cy] center]
    (translate cx cy)
    (line 0 0 x y)))

; And here's the basic mouse-tracking line implementation.
(defn draw-basic-mouse []
  (background 255)
  (let [mouse (v/vec (mouse-x) (mouse-y))
        result (v/- mouse center)]
    (draw-line-from-center result)))

(comment
  (defsketch s-line-basic
    :title "Basic Subtraction of Vectors"
    :draw draw-basic-mouse
    :setup setup-line
    :size [screen-width screen-height]))

; Next comes the half-sized mouse line thing.
(defn draw-half-mouse []
  (background 255)
  (let [mouse (v/vec (mouse-x) (mouse-y))
        result (-> mouse
                 (v/- center)
                 (v/* 0.5))]
    (draw-line-from-center result)))

(comment
  (defsketch s-line-half
    :title "Multiplication of Vectors"
    :draw draw-half-mouse
    :setup setup-line
    :size [screen-width screen-height]))

; Now the mouse tracker with the magnitude bar.
(defn draw-mag-mouse []
  (background 255)
  (let [mouse (v/vec (mouse-x) (mouse-y))
        result (-> mouse
                 (v/- center))
        mag (v/magnitude result)]
    (fill 0)
    (rect 0 0 mag 10)
    (draw-line-from-center result)))

(comment
  (defsketch s-line-mag
    :title "Magnitude of Vectors"
    :draw draw-mag-mouse 
    :setup setup-line
    :size [screen-width screen-height]))

; Normalized vectors.
(defn draw-norm-mouse []
  (background 255)
  (let [mouse (v/vec (mouse-x) (mouse-y))
        result (-> mouse
                 (v/- center)
                 v/normalize
                 (v/* 50))]
    (draw-line-from-center result)))

(comment
  (defsketch s-line-norm
    :title "Normalization of Vectors"
    :draw draw-norm-mouse
    :setup setup-line
    :size [screen-width screen-height]))

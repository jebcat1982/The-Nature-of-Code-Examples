(ns nature-of-code.introduction.walkers
  (:require [roul.random :as r]
            [quil.core :refer [defsketch stroke point background mouse-x
                               mouse-y noise map-range]]
            [quil.helpers.seqs :refer [perlin-noise-seq]]
            [nature-of-code.utils :refer [defrec Renderable render sq]]))


; Define the screen size in one place.
; Unfortunately Quil is broken and can't handle taking something like
; :size some-var in defsketch.  Presumably it's doing some crazy magic at
; macroexpansion time that needs the width and height separately.  So we'll keep
; them separate.  Sigh.
(def screen-size [640 360])
(def screen-width 640)
(def screen-height 360)

; We'll use Clojure records whenever possible instead of the mutable objects the
; book uses.
(defrec Walker [x y])

; Instead of a constructor inside the object, we just use a simple helper
; function.
(defn make-walker []
  (->Walker (/ screen-width 2) (/ screen-height 2)))

; Anything that can draw itself to the screen should implement the Renderable
; protocol.  I would have used the name Drawable, but then the draw function
; inside it would have conflicted with the idiomatic draw function for Quil.
; Naming things is hard.
(extend-type Walker
  Renderable
  (render [{:keys [x y]}]
    (stroke 0)
    (point x y)))

; Let's start with something a bit more readable than the book at first.
; Direction names are easier to read than magic numbers.
(defn step-basic [walker]
  (case (r/rand-nth [:left :right :up :down])
    :left (update-in walker [:x] dec)
    :right (update-in walker [:x] inc)
    :down (update-in walker [:y] inc)
    :up (update-in walker [:y] dec)))

; We'll keep our single walker in a ref to handle state.
(def w (ref nil))

; Not sure why the book says background(0) here.  Wouldn't that make the
; background black?  Or are Quil color numbers different from Processing ones?
; Anyway, we make a fresh Walker here too.
(defn setup []
  (dosync (ref-set w (make-walker)))
  (background 255))

; At each tick we'll update the Walker and render it to the screen, just like
; the book's code.
(defn draw [step-fn]
  (dosync (alter w step-fn))
  (render @w))

; The defsketches are in (comment) forms so they don't all immediately explode
; out when you load the file.
(comment
  (defsketch s-basic
    :title "Initial Walker"
    :setup setup
    :draw (partial draw step-basic)
    :size [screen-width screen-height]))

; This is probably a lot less efficient than the book's code.  We'll see if it
; becomes a problem later on.

; Let's make a little setter for those update-in statements.
(defn move [walker ox oy]
  (-> walker
    (update-in [:x] + ox)
    (update-in [:y] + oy)))

(defn step-diagonal [walker]
  (move walker (r/rand-int -1 2) (r/rand-int -1 2)))

(comment
  (defsketch s-diagonal
    :title "Diagonal Walker"
    :setup setup
    :draw (partial draw step-diagonal)
    :size [screen-width screen-height]))

; Apparently Quil/Processing is okay with getting floats passed to (point)
; instead of integers?  Cool.
(defn step-float [walker]
  (move walker (r/rand -1 1) (r/rand -1 1)))

(comment
  (defsketch s-float
    :title "Floating Point Walker"
    :setup setup
    :draw (partial draw step-float)
    :size [screen-width screen-height]))

; Exercise I.1
; Using Roul's rand-nth-weighted might be cheating.  But since I wrote Roul
; I think it's fair enough.
(defn step-rightish [walker]
  (move walker
        (r/rand-nth-weighted {-1 40
                              0 40
                              1 60})
        (r/rand-nth [-1 0 1])))

(comment
  (defsketch s-rightish
    :title "Rightwards-tending Walker"
    :setup setup
    :draw (partial draw step-rightish)
    :size [screen-width screen-height]))

; The example later on doesn't account for diagonal motion like my answer did.
(defn step-rightish-nondiagonal [walker]
  (apply move walker
         (r/rand-nth-weighted {[-1 0] 20
                               [1 0] 40
                               [0 -1] 20
                               [0 1] 20})))

(comment
  (defsketch s-rightish-nondiagonal
    :title "Rightwards-tending Walker (Non-Diagonal)"
    :setup setup
    :draw (partial draw step-rightish-nondiagonal)
    :size [screen-width screen-height]))

; A step function for a walker that follows the mouse.
(defn step-mouse [{:keys [x y] :as walker}]
  (if (r/rand-bool)
    (step-basic walker)
    (letfn [(towards [dest src]
              (cond (< dest src) -1
                    (> dest src) 1
                    :else 0))]
      (move walker
        (towards (mouse-x) x)
        (towards (mouse-y) y)))))

(comment
  (defsketch s-mouse
    :title "Mouse-chasing Walker"
    :setup setup
    :draw (partial draw step-mouse)
    :size [screen-width screen-height]))


; A step function for a walker that uses a Gaussian distribution.
(defn step-gaussian [walker]
  (move walker
        (r/rand-gaussian)
        (r/rand-gaussian)))

(comment
  (defsketch s-gaussian
    :title "Gaussian Walker"
    :setup setup
    :draw (partial draw step-gaussian)
    :size [screen-width screen-height]))

; First version of the Levy walker.
(defn step-levy-basic [walker]
  (let [spread (if (r/rand-bool 1)
                 [-100 100]
                 [-1 1])]
    (move walker
          (apply r/rand spread)
          (apply r/rand spread))))

(comment
  (defsketch s-levy-basic
    :title "Levy Walker"
    :setup setup
    :draw (partial draw step-levy-basic)
    :size [screen-width screen-height]))


; This looks different than the book's example, but it's not.  The second random
; number from the book is generated in Roul's rand-bool function.
(defn montecarlo []
  (let [v (r/rand)
        prob v]
    (if (r/rand-bool (* 100 prob))
      v
      (recur))))

; Exercise I.6
; First let's split this up into two parts.
(defn rand-biased
  "Generate a random float between 0 and 1.

  prob-fn should be a function that takes a candidate float, and returns the
  probability that it's viable as a float between 0 and 1.

  For example, (rand-biased identity) means that 0.5 has a 50% chance of being
  viable.

  "
  [prob-fn]
  (loop []
    (let [v (r/rand)
          prob (prob-fn v)]
      (if (r/rand-bool (* 100 prob))
        v
        (recur)))))

; Now we can rewrite montecarlo much more cleanly.
(defn montecarlo []
  (rand-biased identity))

; And we can write the exponentially biased random function for exercise I.6
; pretty easily too.
(defn rand-exp
  ([] (rand-biased sq)))

; This part is trickier.  We need to expand the range of the rand-exp output
; from [0, 1] to [-10, 10], but keeping each number's probability of being
; selected proportional to its square.
;
; If we simply did (+ start (* result (- end start))) the positive numbers would
; have too much weight.  Instead, we'll expand the range to [0, 10] and then
; "mirror" it by randomly flipping the result to negative half the time.
;
; This means that zero would be counted twice, but since zero squared is zero
; we'll never get it from rand-exp anyway!
(defn step-biased [walker]
  (letfn [(rand-step-size []
            (let [s (* 10 (rand-exp))]
              (if (r/rand-bool)
                s
                (- s))))]
    (move walker (rand-step-size) (rand-step-size))))

(comment
  (defsketch s-biased
    :title "Biased Walker"
    :setup setup
    :draw (partial draw step-biased)
    :size [screen-width screen-height]))

; Noise Walker example, literally.
(def tx (ref 0))
(def ty (ref 10000))

(defn step-noise [walker]
  (letfn [(noise-incr [t]
            (+ t 0.001))]
    (dosync
      (alter tx noise-incr)
      (alter ty noise-incr)))
  (let [x (map-range (noise @tx) 0 1 0 screen-width)
        y (map-range (noise @ty) 0 1 0 screen-height)]
    (->Walker x y)))

(comment
  (defsketch s-noise
    :title "Perlin Noise Walker"
    :setup setup
    :draw (partial draw step-noise)
    :size [screen-width screen-height]))

; Exercise I.7 - Noise Step Walker
; First let's make a little helper function.
(defn noise-seq
  "Return an infinite lazy noise sequence.

  This function returns an infinite lazy sequence of Perlin noise values between
  the given bounds.

  "
  [incr min-val max-val]
  (map #(map-range % 0 1 min-val max-val)
       (perlin-noise-seq (r/rand) incr)))

; This sequence will be used for the step sizes.  Its values will vary between
; 0 and 8 as Perlin Noise.
(def steps (ref nil))

(defn setup-noise-step []
  (setup)
  (dosync (ref-set steps (noise-seq 0.001 0 8))))

(defn step-noise-step [walker]
  (dosync (alter steps rest))
  (let [s (first @steps)
        ; We know the step size, so now we decide whether to move up, down, left
        ; or right by that much.  There are more efficient ways to do this, but
        ; this way is fine for here.
        [ox oy] (r/rand-nth [[0 s]
                             [0 (- s)]
                             [s 0]
                             [(- s) 0]])]
    (move walker ox oy)))

(comment
  (defsketch s-noise-step
    :title "Perlin Noise Step Walker"
    :setup setup-noise-step
    :draw (partial draw step-noise-step)
    :size [screen-width screen-height]))

(ns nature-of-code.introduction
  (:require [roul.random :as r]
            [quil.core :refer [defsketch stroke point background mouse-x
                               mouse-y noise map-range rect fill no-stroke
                               ellipse smooth noise-detail frame-rate pixels
                               update-pixels color]]
            [quil.helpers.seqs :refer [perlin-noise-seq]]
            [nature-of-code.utils :refer [defrec Renderable render sq
                                          Tickable tick]]))


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
(defrec RandomWalker [x y])

; I'm pulling out the basic drawing functionality into a helper function,
; because most of the walkers we create in this chapter are going to be rendered
; in the same way.
(defn render-walker [{:keys [x y]}]
  (stroke 0)
  (point x y))

; Anything that can draw itself to the screen should implement the Renderable
; protocol.  I would have used the name Drawable, but then the draw function
; inside it would have conflicted with the idiomatic draw function for Quil.
; Naming things is hard.
(extend-type RandomWalker Renderable
  (render [this] (render-walker this)))

; Anything that can be "stepped" or "ticked" once a frame should implement
; Tickable.
(extend-type RandomWalker Tickable
  ; Let's start with something a bit more readable than the book at first.
  ; Direction names are easier to read than magic numbers.
  (tick [this]
    (case (r/rand-nth [:left :right :up :down])
      :left (update-in this [:x] dec)
      :right (update-in this [:x] inc)
      :down (update-in this [:y] inc)
      :up (update-in this [:y] dec))))

; We'll keep our single walker in a ref to handle state.
(def w (ref nil))

; Since setup will be the same for the majority of the stuff in this chapter,
; we'll have a generic setup function.  It will take a function it should use to
; create a Walker (in the center of the screen).
(defn setup-walker [createfn & extra-arguments]
  ; Not sure why the book says background(0) here.  Wouldn't that make the
  ; background black?  Or are Quil color numbers different from Processing ones?
  (background 255)
  (dosync (ref-set w (apply createfn
                            (/ screen-width 2)
                            (/ screen-height 2)
                            extra-arguments))))

; At each tick we'll update the Walker and render it to the screen, just like
; the book's code.  This goes for pretty much every Walker, so we just need
; a single function for this.
(defn draw-walker []
  (dosync (alter w tick))
  (render @w))

; The defsketches are in (comment) forms so they don't all immediately explode
; out when you load the file.
(comment
  (defsketch s-walker-random
    :title "Random Walker"
    :setup (partial setup-walker ->RandomWalker)
    :draw draw-walker
    :size [screen-width screen-height]))

; Let's make a little setter for those update-in statements, otherwise things
; are gonna get kind of ugly.
(defn step [walker ox oy]
  (-> walker
    (update-in [:x] + ox)
    (update-in [:y] + oy)))

; Time to move on to the diagonal walker.
(defrec DiagonalWalker [x y])

; Diagonal walkers aren't rendered any differently.
(extend-type DiagonalWalker Renderable
  (render [this] (render-walker this)))

; Anything that can be "stepped" or "ticked" once a frame should implement
; Tickable.
(extend-type DiagonalWalker Tickable
  ; These rand-int calls are kind of ugly with the 2 in there to bound to 1.
  ; Perhaps I should add a rand-int-inclusive to Roul?
  (tick [this]
    (step this (r/rand-int -1 2) (r/rand-int -1 2))))

; And now the sketch.
; We don't need a special drawing function because our generic draw-walker calls
; tick, which uses the correct Protocol implementation depending on the Walker.
(comment
  (defsketch s-diagonal
    :title "Diagonal Walker"
    :setup (partial setup-walker ->DiagonalWalker)
    :draw draw-walker
    :size [screen-width screen-height]))

; And now the floating-point version.
(defrec FloatWalker [x y])

(extend-type FloatWalker Renderable
  (render [this] (render-walker this)))

(extend-type FloatWalker Tickable
  (tick [this]
    ; We use (rand) instead of (rand-int) here.  That's the only difference.
    (step this (r/rand -1 1) (r/rand -1 1))))

(comment
  (defsketch s-float
    :title "Floating Point Walker"
    :setup (partial setup-walker ->FloatWalker)
    :draw draw-walker
    :size [screen-width screen-height]))


; Time for a brief interlude to graph the output distribution of rand.

; We'll use a ref to track state.
(def random-counts (ref []))

; We'll initialize the ref to a vector instead of a sequence, because we'll need
; to index into it and modify it on every tick.
(defn setup-rand-graph []
  (dosync (ref-set random-counts (vec (repeat 20 0)))))

; Drawing a bar is fairly simple -- it's just a matter of calculating the x, y,
; width, and height of the rectangle.
(defn draw-bar [bar-width bar-number value]
  (let [bx (* bar-number bar-width)
        by (- screen-height value)
        bw bar-width
        bh value]
    (rect bx by bw bh)))

; On each tick, we update a random bucket, and then map over all the buckets to
; draw them.  Pretty simple.
(defn draw-rand-graph []
  ; Clear the screen.
  (background 255)

  ; First we do a bit of calculation so we don't have to do it for every bar.
  (let [bars (count @random-counts)
        bar-width (/ screen-width bars)]

    ; On each tick, we increment a random element of the vector.
    (dosync (alter random-counts update-in [(int (r/rand bars))] inc))

    ; Prepare to draw the bars with a black stroke and grey background.
    (stroke 0)
    (fill 175)

    ; map-indexed is perfect for looping over the bars here.
    (dorun (map-indexed (partial draw-bar bar-width)
                        @random-counts))))

(comment
  (defsketch s-rand
    :title "Distribution of (rand)"
    :setup setup-rand-graph
    :draw draw-rand-graph
    :size [screen-width screen-height]))

; Exercise I.1: Create a random walker that tends to move down and toward the
; right.
(defrec BiasedWalker [x y])

(extend-type BiasedWalker Renderable
  (render [this] (render-walker this)))

(extend-type BiasedWalker Tickable
  (tick [this]
    (step this
          ; Using Roul's rand-nth-weighted might be cheating.  But since I wrote
          ; Roul I think it's fair enough.
          (r/rand-nth-weighted {-1 40  ; 40% chance to go left.
                                0 40   ; 40% chance to not move horizontally.
                                1 60}) ; 60% chance to go right.
          (r/rand-nth-weighted {-1 40     ; 40% chance to go up.
                                0 40      ; 40% chance to not move vertically.
                                1 60})))) ; 60% chance to go down.

(comment
  (defsketch s-biased
    :title "Biased Walker"
    :setup (partial setup-walker ->BiasedWalker)
    :draw draw-walker
    :size [screen-width screen-height]))

; Exercise I.2: What is the probability of drawing two aces in a row from a deck
; of 52 cards?
(* (/ 4 52) (/ 3 51))

; The next example in the book is a simple form of the biased walker we made in
; exercise I.1 so I won't remake it here.

; Exercise I.3: A walker that has a 50% chance to walk toward the mouse.
(defrec MouseWalker [x y])

(extend-type MouseWalker Renderable
  (render [this] (render-walker this)))

; This function uses a bunch of internal helper functions.  I find it easier to
; read when written like this.
(extend-type MouseWalker Tickable
  (tick [this]
    (letfn [(step-random [walker]
              (step walker (r/rand -1 1) (r/rand -1 1)))
            (towards [dest src]
              (cond (< dest src) -1
                    (> dest src) 1
                    :else 0))
            (step-toward-mouse [{:keys [x y] :as walker}]
              (step walker
                    (towards (mouse-x) x)
                    (towards (mouse-y) y)))]
      (if (r/rand-bool)
        (step-random this)
        (step-toward-mouse this)))))

(comment
  (defsketch s-mouse
    :title "Mouse Walker"
    :setup (partial setup-walker ->MouseWalker)
    :draw draw-walker
    :size [screen-width screen-height]))

; Next we're back to distribution stuff.  This time: Gaussian distributions.
(defn setup-gaussian-circles []
  (no-stroke)
  (fill 0 10)
  (background 255))

(defn draw-gaussian-circles []
  ; Again, this is a bit of a cheat, but I wrote rand-gaussian in Roul so my
  ; conscience is clear!
  (ellipse (r/rand-gaussian 320 60)
           (/ screen-height 2)
           16 16))

(comment
  (defsketch s-gaussian
    :title "Gaussian Distribution"
    :setup setup-gaussian-circles
    :draw draw-gaussian-circles
    :size [screen-width screen-height]))

; Exercise I.4: Paint splatter!
(defn setup-splatter []
  (background 255)
  (no-stroke)
  (smooth))

; The x and y coordinates for each drop will be plucked from a Gaussian
; distribution with its mean at the center of the screen, and its standard
; deviation at 1/7 the width of the screen.  This means that 99.7% of the drops
; should be within a 3/7s-screen-wide radius of the center.
(defn draw-splatter []
  (let [x (r/rand-gaussian (/ screen-width 2)
                           (/ screen-width 7))
        y (r/rand-gaussian (/ screen-height 2)
                           (/ screen-height 7))
        ; We'll also make the radius a bit random, mostly clustering around 5px,
        ; and clamped to zero and 10 (because a negative radius doesn't make any
        ; sense).
        radius (r/rand-gaussian 5 1
                                0 10)
        ; Finally we'll determine the red in the RGB value randomly as well
        ; (clamped to valid values of course).
        red (r/rand-gaussian 150 40 0 255)]
    (fill red 0 0)
    (ellipse x y radius radius)))

(comment
  (defsketch s-splatter
    :title "Paint Splatter"
    :setup setup-splatter
    :draw draw-splatter
    :size [screen-width screen-height]))

; Exercise I.5: Back to walkers, this time a Gaussian one.
(defrec GaussianWalker [x y])

(extend-type GaussianWalker Renderable
  (render [this] (render-walker this)))

(extend-type GaussianWalker Tickable
  (tick [this]
    (step this
          (r/rand-gaussian) ; rand-gaussian has a mean/sd of 0/1 by default
          (r/rand-gaussian))))

(comment
  (defsketch s-gaussian
    :title "Gaussian Walker"
    :setup (partial setup-walker ->GaussianWalker)
    :draw draw-walker
    :size [screen-width screen-height]))

; Next comes the basic Levy flight walker.  It has a 1% chance of taking a large
; step.
(defrec BasicLevyWalker [x y])

(extend-type BasicLevyWalker Renderable
  (render [this] (render-walker this)))

(extend-type BasicLevyWalker Tickable
  (tick [this]
    ; (rand-bool n) has an n% chance of returning true, so it's perfect here.
    (let [spread (if (r/rand-bool 1)
                   [-100 100]
                   [-1 1])]
      (step this
            (apply r/rand spread)
            (apply r/rand spread)))))

(comment
  (defsketch s-basic-levy
    :title "Basic Levy Walker"
    :setup (partial setup-walker ->BasicLevyWalker)
    :draw draw-walker
    :size [screen-width screen-height]))

; The Montecarlo function from the book.
; This looks different than the book's example, but it's not.  The second random
; number in the book is generated here by Roul's rand-bool function.
(defn montecarlo []
  (let [v (r/rand) ; Generate the candidate.
        prob v]    ; Generate the probability it's acceptable.
    (if (r/rand-bool (* 100 prob))
      v
      (recur))))

; Exercise I.6:
; Use a custom probability distribution to vary the size of a step taken by the
; random walker. The step size can be determined by influencing the range of
; values picked. Can you map the probability exponentially—i.e. making the
; likelihood that a value is picked equal to the value squared?

; First let's split this up into two parts.  We'll make a utility function to
; keep things cleaner.
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

; As a bonus, we can rewrite montecarlo much more cleanly.
(defn montecarlo []
  (rand-biased identity))

; And we can write the exponentially biased random function for exercise I.6
; pretty easily too.
(defn rand-exp []
  (rand-biased sq))

; This next part is trickier.  We need to expand the range of the rand-exp
; output from [0, 1] to [-10, 10], but keeping each number's probability of
; being selected proportional to its square.
;
; If we simply expanded and shifted the result by doing something like (+ start
; (* result (- end start))) the positive numbers would have too much weight.
;
; Instead, we'll expand the range to [0, 10] and then "mirror" it by randomly
; flipping the result to negative half the time.
;
; This means that zero would be counted twice, but since zero squared is zero
; we'll never get it from rand-exp anyway!
(defrec ExpWalker [x y])

(extend-type ExpWalker Renderable
  (render [this] (render-walker this)))

(extend-type ExpWalker Tickable
  (tick [this]
    (letfn [(rand-step-size []
              (let [n (rand-exp)  ; Get a random (exp. biased) number [0, 1].
                    n (* 10 n)]   ; Expand the range to [0, 10].
                (if (r/rand-bool) ; Half the time, flip it.
                  n
                  (- n))))]
      (step this (rand-step-size) (rand-step-size)))))

(comment
  (defsketch s-exp
    :title "Exponential Distribution Walker"
    :setup (partial setup-walker ->ExpWalker)
    :draw draw-walker
    :size [screen-width screen-height]))

; Next topic: Perlin Noise

; First up is a ball moving accoring to Perlin Noise.  I'll store the time as
; the state in a ref.
(def t (ref nil))

(defn setup-noise-ball []
  (dosync (ref-set t 0))
  (smooth)
  (fill 200)
  (stroke 0))

; Nothing too crazy going on here.  On each tick we grab the noise at the
; current time to determine where (horizontally) to draw the ball.  We draw it,
; then increment the noise before moving on.
(defn draw-noise-ball []
  (background 255)
  (let [n (noise @t)
        x (map-range n 0 1 0 screen-width)
        y (/ screen-height 2)]
    (ellipse x y 20 20))
  (dosync (alter t + 0.005)))

(comment
  (defsketch s-noise-ball
    :title "Perlin Noise Ball"
    :setup setup-noise-ball
    :draw draw-noise-ball
    :size [screen-width screen-height]))


; Next we have the Perlin Noise -based walkers.  We'll start with a literal
; translation of the example which ends up being pretty ugly in Clojure.

; We'll keep the noise times stored in the walker itself so we don't need to
; screw around with more refs.
(defrec PerlinWalker [x y tx ty])

(extend-type PerlinWalker Renderable
  (render [this] (render-walker this)))

; We're not using the step helper function here, because we're not moving by
; offsets any more (we're moving to absolute positions based on the mapped
; Perlin noise).
;
; Also I could have used a bunch of update-in's here, but since we're changing
; literally every attribute of the walker it's easiest to just make a new one.
(extend-type PerlinWalker Tickable
  (tick [{:keys [tx ty] :as this}]
    (->PerlinWalker
        (map-range (noise tx)
                   0 1
                   0 screen-width)
        (map-range (noise ty)
                   0 1
                   0 screen-height)
        (+ tx 0.001)
        (+ ty 0.001))))

; We finally need to use the extra-arguments part of the setup function.
;
; Yes, technically the walker will be at the center of the screen and then
; "jump" to the first location the Perlin noise specifies.  But the drawing
; function actually ticks *before* rendering, so you'll never see that initial
; center dot appear on the screen.  Whew!
(comment
  (defsketch s-perlin
    :title "Perlin Noise Walker"
    :setup (partial setup-walker ->PerlinWalker 0 10000)
    :draw draw-walker
    :size [screen-width screen-height]))


; Exercise I.7: A walker with a step size based on Perlin noise.
; First let's make a little helper function.  noise-seq will produce a lazy,
; infinite sequence of values for us.
;
; The values will come from a Perlin noise sequence with incr as the time
; increment and a random seed.
;
; The values will be mapped to the given interval as well.  For example:
;
;     (noise-seq 0.001 0 8)
;
; will return an infinite, lazy sequence of values between 0 and 8 from a Perlin
; sequence with a random seen and a time increment of 0.001.
;
; Handy!
(defn noise-seq [incr min-val max-val]
  (map #(map-range % 0 1 min-val max-val)
       (perlin-noise-seq (r/rand-int 10000) incr)))


; The walker will hold two infinite streams (one for x, one for y) of step sizes
; itself.
(defrec NoiseyWalker [x y x-step-sizes y-step-sizes])

; Renders normally, nothing special here.
(extend-type NoiseyWalker Renderable
  (render [this] (render-walker this)))

; Each tick we need to move the walker we'll not only move it, but we'll chop an
; element off of each step-size stream as well.  Destructuring makes this pretty
; concise.
(extend-type NoiseyWalker Tickable
  (tick [{[ox & x-step-sizes] :x-step-sizes
          [oy & y-step-sizes] :y-step-sizes
          :as this}]
    (-> this
      (step ox oy)
      (assoc-in [:x-step-sizes] x-step-sizes)
      (assoc-in [:y-step-sizes] y-step-sizes))))

; This walker has a tendency to run off the screen. It might be worth playing
; with the min/max step values and the time increment to see if we could fix
; that.  Or we could just clamp the sucker to the edges.
(comment
  (defsketch s-noisey
    :title "Noisey Step Walker"
    :setup (partial setup-walker ->NoiseyWalker
                    (noise-seq 0.01 -4 4)
                    (noise-seq 0.01 -4 4))
    :draw draw-walker
    :size [screen-width screen-height]))

; Now for the 2D noise drawing.  We'll reuse the same time ref from the noise
; ball.
(defn setup-noise-2d []
  (dosync (ref-set t 0))
  (noise-detail 8 0.45)
  (frame-rate 15)
  (background 255))

; We're not even past the Introduction and we're at the point where Clojure gets
; ugly.  Type hinting ahoy!
(defn draw-noise-2d []
  ; Increment the time.
  (dosync (alter t + 0.05))

  ; Now we get to write to the pixels array.  If you try to do this with the
  ; normal Quil functions Clojure is going to eat your CPU, so instead we'll
  ; deal with the godawful Java interop.
  (let [^ints screen-pixels (pixels)
        t @t]
    ; We'll loop over every x, y coordinate.
    (doseq [x (range screen-width)
            y (range screen-height)
            :let [xoff (* 0.010 x) ; We scale down the x and y coordinates, just
                  yoff (* 0.010 y) ; like the book.

                  ; Get the noise value at the (x, y, time) coordinates.
                  noise-val (noise xoff yoff t)

                  ; Turn the noise value (which is from 0 to 1) into
                  ; a brightness value.
                  brightness (int (color (map-range noise-val
                                                    0 1
                                                    0 255)))

                  ; Find the index of the target pixel in the pixels array.
                  pi (int (+ x (* screen-width y)))]]

      ; Set the array value.  With all the type hinting above this *should*
      ; avoid reflection and be slightly less horrifyingly slow.
      (aset screen-pixels pi brightness)))

  ; Processing/Quil require you to call update-pixels when you're done poking
  ; around in the pixels array.
  (update-pixels))

; Beware: this will eat your CPU.  Don't leave it running if you're on battery
; power.
(comment
  (defsketch s-noise-2d
    :title "Perlin Noise 2D"
    :setup setup-noise-2d
    :draw draw-noise-2d
    :renderer :p2d
    :size [screen-width screen-height]))

; I don't really understand the landscape exercise, so I'll skip that one for
; now.

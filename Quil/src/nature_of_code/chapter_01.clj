(ns nature-of-code.chapter-01
  (:require [quil.core :refer [defsketch background smooth ellipse stroke
                               fill frame-rate stroke-weight point translate
                               line mouse-x mouse-y rect map-range]]
            [quil.helpers.seqs :refer [perlin-noise-seq]]
            [roul.random :as r]
            [nature-of-code.utils :refer [Renderable render Tickable tick
                                          defrec get-key sq]]
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
; We'll make a mouse-vector helper function because we're going to need it
; often.
(defn mouse-vector []
  (v/vec (mouse-x) (mouse-y)))

; And now the basic mouse drawer is trivial.
(defn draw-basic-mouse []
  (background 255)
  (let [mouse (mouse-vector)
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
  (draw-line-from-center
    (-> (mouse-vector)
      (v/- center)
      (v/* 0.5))))

(comment
  (defsketch s-line-half
    :title "Multiplication of Vectors"
    :draw draw-half-mouse
    :setup setup-line
    :size [screen-width screen-height]))

; Now the mouse tracker with the magnitude bar.
(defn draw-mag-mouse []
  (background 255)
  (let [result (-> (mouse-vector)
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
  (draw-line-from-center
    (-> (mouse-vector)
      (v/- center)
      v/normalize
      (v/* 50))))

(comment
  (defsketch s-line-norm
    :title "Normalization of Vectors"
    :draw draw-norm-mouse
    :setup setup-line
    :size [screen-width screen-height]))

; The next part of the chapter is about making a generic mover class than
; cascades acceleration -> velocity -> location.  We'll continue to use records
; and functions instead of objects with mutable state.
(def m (ref nil))

; First off, we'll create a helper function to handle the "wrap around the
; edges" behavior cleanly.
(defn wrap-bounds
  "Handle wrapping the components of a location vector around the screen bounds."
  [[x y]]
  (letfn [(wrap [n max]
            (cond (neg? n) max
                  (> n max) 0
                  :else n))]
    (v/vec
      (wrap x screen-width)
      (wrap y screen-height))))

; And a helper for a vector to a random place in the screen.
(defn random-location []
  (v/vec (r/rand screen-width)
         (r/rand screen-height)))

(defrec Mover101 [location velocity])

; Movers are just rendered as nornal balls, nothing crazy.
(extend-type Mover101 Renderable
  (render [{[x y] :location}]
    (render-ball x y)))

; Updating is a bit trickier.  I'm going to try to illustrate what the book
; does, but still keep the examples separate.
;
; The goal of the book is to have the "update the mover" stuff encapsulated into
; a single update-function.  The first version is going to be different though,
; so I'll give it its own name.
(defn update-mover-101 [[location velocity :as mover]]
  (-> mover
    (update-in [:location] v/+ velocity)
    (update-in [:location] wrap-bounds)))

; Right now this is trivial, but you'll see this tick function become more
; useful later on.
(extend-type Mover101 Tickable
  (tick [this]
    (update-mover-101 this)))

; Boilerplate.
(defn setup-mover [fresh-mover]
  (stroke 0)
  (fill 175)
  (stroke-weight 2)
  (background 255)
  (smooth)
  (dosync (ref-set m fresh-mover)))

; Boilerplate.  Perhaps it's worth writing a function for this:
;
; (defn clear-tick-draw [r]
;   (background 255)
;   (dosync (alter r tick))
;   (render @r))
;
; That function would work for a bunch of different things.  But it also
; wouldn't work for some others, so it's probably not worth it at this point.
(defn draw-mover []
  (background 255)
  (dosync (alter m tick))
  (render @m))

; Let's just draw this basic mover and move on to something more interesting.
(comment
  (defsketch s-mover101
    :title "Mover101"
    :setup (partial setup-mover
                    (->Mover101 (random-location)
                                (v/vec (r/rand -2 2)
                                       (r/rand -2 2))))
    :draw draw-mover
    :size [screen-width screen-height]))


; Acceleration!
(defrec AccelerationMover [location velocity acceleration])

(extend-type AccelerationMover Renderable
  (render [{[x y] :location}]
    (render-ball x y)))

; Now here's where the distinction between the update and tick function starts
; to show up.  Update is going to take an acceleration and handle cascading it
; all the way down.  Tick is going to determine that acceleration.
;
; Unfortunately there's no nicer way to thread this that I can see, because the
; update-in for locations needs to access the *new* velocity.  For example, this
; looks nice but doesn't work:
;
; (-> mover
;   (update-in [:velocity] v/+ acceleration)
;   (update-in [:location] v/+ ,,,))
;
; The problem is that that second update-in needs the updated velocity from the
; first update-in.  Oh well.
(defn update-mover [mover acceleration]
  (let [mover (update-in mover [:velocity] v/+ acceleration)
        mover (update-in mover [:velocity] v/limit 10)
        mover (update-in mover [:location] v/+ (:velocity mover))
        mover (update-in mover [:location] wrap-bounds)]
    mover))

; Now that we have a generic update-mover function, our tick function is only
; going to be responsible for computing acceleration.
;
; For this first mover, the acceleration is simply a constant stored in the
; mover itself.
(extend-type AccelerationMover Tickable
  (tick [{:keys [acceleration] :as this}]
    (update-mover this acceleration)))


; And now we draw it.
(comment
  (defsketch s-acceleration-mover
    :title "Acceleration Mover"
    :setup (partial setup-mover
                    (->AccelerationMover (random-location)
                                         (v/vec 0 0)
                                         (v/vec -0.001 0.01)))
    :draw draw-mover
    :size [screen-width screen-height]))

; Exercise 1.5: Car simulation.  A Car is a Mover just like the previous one.
(defrecord Car [location velocity])

; We'll render it as a rectangle, just for fun.
(extend-type Car Renderable
  (render [{[x y] :location}]
    (stroke 0)
    (fill 175)
    (rect x y 16 16)))

; We'll keep the state in a couple of refs.
(def c (ref nil))
(def is-braking (ref nil))
(def is-accelerating (ref nil))

; A couple of helper functions to adjust an acceleration vector for the gas and
; brake "pedals."
(defn gas [acc]
  (if @is-accelerating
    (v/+ (v/vec 0.2 0.2) acc)
    acc))

(defn brake [acc]
  (if @is-braking
    (v/- acc (v/vec 0.1 0.1))
    acc))

; The setup function will initialize the refs for us.
(defn setup-car []
  (frame-rate 30)
  (dosync
    (ref-set is-accelerating false)
    (ref-set is-braking false)
    (ref-set c (->Car center
                      (v/vec 0 0)))))

; Tick is responsible for calculating the acceleration.  update-mover handles
; everything else sanely.
(extend-type Car Tickable
  (tick [this]
    (let [acceleration (-> (v/vec 0 0) gas brake)]
      (update-mover this acceleration))))

; The Quil draw function is nothing special.
(defn draw-car []
  (background 255)
  (dosync (alter c tick))
  (render @c))

; Now comes handling the keystrokes.  This is surprisingly painful in Quil.
; I've split out the bullshit parts into utils/get-key if you're curious.
(defn key-pressed-car []
  (let [k (get-key)]
    (case k
      :up (dosync (ref-set is-accelerating true))
      :down (dosync (ref-set is-braking true)))))

(defn key-released-car []
  (let [k (get-key)]
    (case k
      :up (dosync (ref-set is-accelerating false))
      :down (dosync (ref-set is-braking false)))))

; And now we're ready.  The sketch itself just assembles the stuff we've made.
;
; Note that this isn't really "gas and brake" as much as "forward and back".  If
; you "brake" enough you'll start going backwards.  I should figure out how to
; fix this, but it's probably a bit more work.
(comment
  (defsketch s-car
    :title "Car"
    :setup setup-car
    :draw draw-car
    :key-pressed key-pressed-car
    :key-released key-released-car
    :size [screen-width screen-height]))

; Enough of the Car, on to a random mover.  This is really concise thanks to all
; the helper functions we've already made.
(defrec RandomMover [location velocity])

(extend-type RandomMover Renderable
  (render [{[x y] :location}]
    (render-ball x y)))

(extend-type RandomMover Tickable
  (tick [this]
    (update-mover this  (v/vec (r/rand -1 1)
                               (r/rand -1 1)))))

(comment
  (defsketch s-random-mover
    :title "Random Mover"
    :setup (partial setup-mover
                    (->RandomMover (random-location)
                                   (v/vec 0 0)))
    :draw draw-mover
    :size [screen-width screen-height]))

; Exercise 1.6: Perlin Mover!

; We'll start by copying over our old friend from the introduction: noise-seq.
; It may be worth putting this one into utils.
(defn noise-seq [incr min-val max-val]
  (map #(map-range % 0 1 min-val max-val)
       (perlin-noise-seq (r/rand-int 10000) incr)))

; Once again we'll use the strategy of letting the record keep its infinite list
; of random values inside itself.
(defrecord PerlinMover [location velocity accelerations])

(extend-type PerlinMover Renderable
  (render [{[x y] :location}]
    (render-ball x y)))

(extend-type PerlinMover Tickable
  (tick [{[acc & accs] :accelerations :as this}]
    (-> this
      (update-mover acc)
      (assoc :accelerations accs))))

(defn random-perlin-vectors
  "Return a lazy, infinite sequence of perlin-noisey vectors."
  [incr min max]
  (map v/vec
       (noise-seq incr min max)
       (noise-seq incr min max)))

(comment
  (defsketch s-perlin-mover
    :title "Perlin Mover"
    :setup (partial setup-mover
                    (->PerlinMover (random-location)
                                   (v/vec 0 0)
                                   (random-perlin-vectors 0.01 -1 1)))
    :draw draw-mover
    :size [screen-width screen-height]))

; The entire static/non-static section of this chapter doesn't apply here
; because all vectors are immutable.

; On to the mouse mover!
(defrecord MouseMover [location velocity])

(extend-type MouseMover Renderable
  (render [{[x y] :location}]
    (render-ball x y)))

; This is a pretty straightforward translation.
(extend-type MouseMover Tickable
  (tick [{:keys [location] :as this}]
    (let [direction (v/- (mouse-vector) location)
          acceleration (-> direction
                         v/normalize
                         (v/* 0.5))]
      (update-mover this acceleration))))

(comment
  (defsketch s-mouse
    :title "Mouse Mover"
    :setup (partial setup-mover
                    (->MouseMover center (v/vec 0 0)))
    :draw draw-mover
    :size [screen-width screen-height]))

; Exercise 1.8: A mouse mover with variable attraction.  I'll make the
; attraction stronger when the object is closer to the mouse.
(defrecord VariableMover [location velocity])

(extend-type VariableMover Renderable
  (render [{[x y] :location}]
    (render-ball x y)))

; This is a bit ugly, but shouldn't be too hard to follow.
(extend-type VariableMover Tickable
  (tick [{:keys [location] :as this}]
    (let [maximum-magnitude (v/magnitude (v/vec screen-width screen-height))
          direction (v/- (mouse-vector) location)
          distance (/ maximum-magnitude (v/magnitude direction))
          acceleration (-> direction
                         v/normalize
                         (v/* (sq distance) 0.01))]
      (update-mover this acceleration))))

(comment
  (defsketch s-variable-mouse-mover
    :title "Variable Mover"
    :setup (partial setup-mover
                    (->VariableMover center (v/vec 0 0)))
    :draw draw-mover
    :size [screen-width screen-height]))

; Next is the array of many mouse movers.
(def ms (ref nil))

(defn random-mouse-mover []
  (->MouseMover (random-location) (v/vec 0 0)))

(defn setup-mice []
  (stroke 0)
  (fill 175)
  (stroke-weight 2)
  (background 255)
  (smooth)
  (dosync (ref-set ms (repeatedly 20 random-mouse-mover))))

(defn draw-mice []
  (background 255)
  (dosync (alter ms #(map tick %)))
  (dorun (map render @ms)))


(comment
  (defsketch s-mice
    :title "Many Mouse Movers"
    :setup setup-mice
    :draw draw-mice
    :size [screen-width screen-height]))

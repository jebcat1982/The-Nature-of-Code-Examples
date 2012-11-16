(ns nature-of-code.chapter-01.movers
  (:require [roul.random :as r]
            [quil.core :refer [defsketch stroke point background frame-rate fill
                               ellipse text key-code raw-key rect smooth
                               mouse-x mouse-y]]
            [quil.helpers.seqs :refer [perlin-noise-seq]]
            [nature-of-code.vectors :as v]
            [nature-of-code.utils :refer [defrec Renderable render Tickable
                                          tick get-key]]))

(def screen-width 640)
(def screen-height 360)

(defn bound-cross
  "Calculate the proper location."
  [x max-x]
  (cond
    (<= x 0) max-x
    (>= x max-x) 0
    :else x))

(defn update-mover [{:keys [velocity top-speed] :as mover} acceleration]
  (-> mover
    (update-in [:velocity] v/+ acceleration)
    (update-in [:velocity] v/limit top-speed)
    (update-in [:location] v/+ velocity)
    (update-in [:location :x] bound-cross screen-width)
    (update-in [:location :y] bound-cross screen-height)))


(defrecord Mover [location velocity acceleration top-speed])

(extend-type Mover Renderable
  (render [{[x y] :location}]
    (stroke 0)
    (fill 175)
    (ellipse x y 16 16)))

(extend-type Mover Tickable
  (tick [{:keys [acceleration] :as this}]
    (update-mover this acceleration)))


(defn make-mover []
  (map->Mover {:location (v/vec (/ screen-width 2) (/ screen-height 2))
               :velocity (v/vec 0 0)
               :acceleration (v/vec -0.001 0.01)
               :top-speed 10}))

(def m (ref nil))

(defn setup []
  (frame-rate 30)
  (dosync (ref-set m (make-mover))))

(defn status []
  (let [[x y] (:location @m)
        [vx vy] (:velocity @m)]
    (text (format "location: %.1f, %.1f" (float x) (float y)) 5 16)
    (text (format "velocity: %.3f, %.3f" (float vx) (float vy)) 5 32)))

(defn draw []
  (dosync (alter m tick))
  (background 255)
  (status)
  (render @m))

(comment
  (defsketch s-mover
    :title "Vectored Mover"
    :setup setup
    :draw draw
    :size [screen-width screen-height]))

(defrecord Car [location velocity top-speed])

(def c (ref nil))
(def keypress (ref nil))
(def is-braking (ref nil))
(def is-accelerating (ref nil))

(extend-type Car Renderable
  (render [{[x y] :location}]
    (stroke 0)
    (fill 175)
    (rect x y 16 16)))

(defn accelerate-car [acc]
  (if @is-accelerating
    (v/+ (v/vec 0.2 0.2) acc)
    acc))

(defn brake-car [acc]
  (if @is-braking
    (v/- acc (v/vec 0.1 0.1))
    acc))

(extend-type Car Tickable
  (tick [{:keys [velocity top-speed] :as this}]
    (let [acceleration (-> (v/vec 0 0) accelerate-car brake-car)]
      (-> this
        (update-in [:velocity] v/+ acceleration)
        (update-in [:velocity] v/limit top-speed)
        (update-in [:location] v/+ velocity)
        (update-in [:location :x] bound-cross screen-width)
        (update-in [:location :y] bound-cross screen-height)))))


(defn make-car []
  (map->Car {:location (v/vec (/ screen-width 2) (/ screen-height 2))
             :velocity (v/vec 0 0)
             :top-speed 10}))


(defn status-car []
  (text (format "last keypress: %s" (prn-str @keypress)) 5 16)
  (when @is-braking
    (text (format "braking") 5 32))
  (when @is-accelerating
    (text (format "accelerating") 30 32)))

(defn setup-car []
  (frame-rate 30)
  (dosync
    (ref-set keypress nil)
    (ref-set is-accelerating false)
    (ref-set is-braking false)
    (ref-set c (make-car))))

(defn draw-car []
  (background 255)
  (status-car)
  (dosync (alter c tick))
  (render @c))

(defn key-pressed-car []
  (let [k (get-key)]
    (dosync (ref-set keypress k))
    (case k
      :up (dosync (ref-set is-accelerating true))
      :down (dosync (ref-set is-braking true)))))

(defn key-released-car []
  (let [k (get-key)]
    (case k
      :up (dosync (ref-set is-accelerating false))
      :down (dosync (ref-set is-braking false)))))

(comment
  (defsketch s-car
    :title "Car"
    :setup setup-car
    :draw draw-car
    :key-pressed key-pressed-car
    :key-released key-released-car
    :size [screen-width screen-height]))

(defrecord RandomMover [location velocity top-speed])

(extend-type RandomMover Renderable
  (render [{[x y] :location}]
    (stroke 0)
    (fill 175)
    (ellipse x y 16 16)))

(extend-type RandomMover Tickable
  (tick [this]
    (let [acceleration (v/random-vector)]
      (update-mover this acceleration))))

(defn make-random-mover []
  (map->RandomMover {:location (v/vec (/ screen-width 2) (/ screen-height 2))
                     :velocity (v/vec 0 0)
                     :top-speed 10}))

(defn setup-random []
  (smooth)
  (frame-rate 30)
  (dosync (ref-set m (make-random-mover))))

(defn draw-random []
  (dosync (alter m tick))
  (background 255)
  (render @m))

(comment
  (defsketch s-random
    :title "Random Mover"
    :setup setup-random
    :draw draw-random
    :size [screen-width screen-height]))

(defn scale-noise [min max n]
  (let [width (- max min)]
    (+ min (* width n))))

(defn scaled-perlin-noise-seq [min max seed incr]
  (map (partial scale-noise min max)
       (perlin-noise-seq seed incr)))

(defn random-perlin-noise-seq [min max incr]
  (scaled-perlin-noise-seq min max (r/rand 10000) incr))

(defrecord PerlinMover [location velocity accelerations top-speed])

(extend-type PerlinMover Renderable
  (render [{[x y] :location}]
    (stroke 0)
    (fill 175)
    (ellipse x y 16 16)))

(extend-type PerlinMover Tickable
  (tick [{:keys [accelerations] :as this}]
    (let [acceleration (first accelerations)]
      (-> this
        (update-mover acceleration)
        (update-in [:accelerations] rest)))))

(defn make-perlin-mover []
  (map->PerlinMover {:location (v/vec (/ screen-width 2) (/ screen-height 2))
                     :velocity (v/vec 0 0)
                     :accelerations (map v/vec
                                         (random-perlin-noise-seq -1 1 0.1)
                                         (random-perlin-noise-seq -1 1 0.1))
                     :top-speed 10}))

(defn setup-perlin []
  (smooth)
  (frame-rate 30)
  (dosync (ref-set m (make-perlin-mover))))

(defn draw-perlin []
  (dosync (alter m tick))
  (background 255)
  (render @m))

(comment
  (defsketch s-perlin
    :title "Perlin Mover"
    :setup setup-perlin
    :draw draw-perlin
    :size [screen-width screen-height]))

; The entire static/non-static section doesn't apply because all PVectors here
; are immutable.

(defrecord MouseMover [location velocity top-speed])

(extend-type MouseMover Renderable
  (render [{[x y] :location}]
    (stroke 0)
    (fill 175)
    (ellipse x y 16 16)))

(extend-type MouseMover Tickable
  (tick [{:keys [location] :as this}]
    (let [mouse (v/vec (mouse-x) (mouse-y))
          direction (v/- mouse location)
          acceleration (-> direction
                         v/normalize
                         (v/* 0.5))]
      (update-mover this acceleration))))

(defn make-mouse-mover []
  (map->MouseMover {:location (v/vec (/ screen-width 2) (/ screen-height 2))
                    :velocity (v/vec 0 0)
                    :top-speed 10}))


(defn setup-mouse []
  (smooth)
  (frame-rate 30)
  (dosync (ref-set m (make-mouse-mover))))

(defn draw-mouse []
  (dosync (alter m tick))
  (background 255)
  (render @m))

(comment
  (defsketch s-mouse
    :title "Mouse Mover"
    :setup setup-mouse
    :draw draw-mouse
    :size [screen-width screen-height]))


(defrecord VariableMover [location velocity top-speed])

(extend-type VariableMover Renderable
  (render [{[x y] :location}]
    (stroke 0)
    (fill 175)
    (ellipse x y 16 16)))

(extend-type VariableMover Tickable
  (tick [{:keys [location] :as this}]
    (let [variable (v/vec (mouse-x) (mouse-y))
          direction (v/- variable location)
          acceleration (v/* direction 0.01)]
      (update-mover this acceleration))))

(defn make-variable-mover []
  (map->VariableMover {:location (v/vec (/ screen-width 2) (/ screen-height 2))
                    :velocity (v/vec 0 0)
                    :top-speed 10}))


(defn setup-variable []
  (smooth)
  (frame-rate 30)
  (dosync (ref-set m (make-variable-mover))))

(defn draw-variable []
  (dosync (alter m tick))
  (background 255)
  (render @m))

(comment
  (defsketch s-variable
    :title "Variable Mover"
    :setup setup-variable
    :draw draw-variable
    :size [screen-width screen-height]))

(def ms (ref nil))

(defn make-mouse-mover-random []
  (map->MouseMover {:location (v/vec (r/rand screen-width)
                                     (r/rand screen-height))
                    :velocity (v/vec 0 0)
                    :top-speed 10}))

(defn setup-mouses []
  (smooth)
  (frame-rate 30)
  (dosync (ref-set ms (repeatedly 20 make-mouse-mover-random))))

(defn draw-mouses []
  (dosync (alter ms #(map tick %)))
  (background 255)
  (dorun (map render @ms)))

(comment
  (defsketch s-mouses
    :title "Mouse Movers"
    :setup setup-mouses
    :draw draw-mouses
    :size [screen-width screen-height]))

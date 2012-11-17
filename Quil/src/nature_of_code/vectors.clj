(ns nature-of-code.vectors
  (:refer-clojure :exclude [+ - * / vec])
  (:require [nature-of-code.utils :refer [defrec sqrt sq]]
            [roul.random :as r]))


(defrec PVector [x y])

(defn +
  "Add the given vectors.

  With no arguments, returns a vector of [0, 0].

  With one argument, returns that vector.

  With two or more arguments, adds them and returns the result.

  "
  ([]
   (->PVector 0 0))
  ([v]
   v)
  ([[x1 y1] [x2 y2]]
   (->PVector (clojure.core/+ x1 x2)
              (clojure.core/+ y1 y2)))
  ([v1 v2 & more]
   (reduce + (+ v1 v2) more)))

(defn -
  "Subtract the given vectors.

  With no arguments, returns a vector of [0, 0].

  With one argument, returns that vector negated.

  With two or more arguments, subtracts them and returns the result.

  "
  ([]
   (->PVector 0 0))
  ([[x1 y1]]
   (->PVector (clojure.core/- x1)
              (clojure.core/- y1)))
  ([[x1 y1] [x2 y2]]
   (->PVector (clojure.core/- x1 x2)
              (clojure.core/- y1 y2)))
  ([v1 v2 & more]
   (reduce - (- v1 v2) more)))

(defn *
  "Scale the vector by the given constant amount.
  
  With no arguments, returns a vector of [1, 1].

  With one argument, returns that vector.

  With two or more arguments, scales the vector by the given constants and
  returns the result.

  "
  ([]
   (->PVector 1 1))
  ([v]
   v)
  ([[x y] c] 
   (->PVector (clojure.core/* c x)
              (clojure.core/* c y)))
  ([v c & more]
   (reduce * (* v c) more)))

(defn /
  "Scale the vector by the inverse of the given constant amount."
  [[x y] c]
  (->PVector (clojure.core// x c)
             (clojure.core// y c)))


(defn magnitude
  "Return the magnitude of the given vector."
  [[x y]]
  (sqrt (clojure.core/+ (sq x) (sq y))))

(defn normalize
  "Normalize the given vector into a unit vector.
  
  If the vector has a magnitude of 0, return it unchanged.

  "
  [v]
  (let [m (magnitude v)]
    (if (zero? m)
      v
      ; For some reason we still get a divide by zero error here very rarely.
      ; Floating point fun?
      (/ v m))))

(defn limit
  "Limit the magnitude of the given vector to the given constant value."
  ([v max]
   (if (< (magnitude v) max)
     v
     (* (normalize v) max))))

(defn random-vector
  "Return a randomized unit vector."
  []
  (->PVector (r/rand -1 1) (r/rand -1 1)))


(def vec ->PVector)

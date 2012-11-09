(ns nature-of-code.utils)


(defmacro defrec
  "Define a Record like defrecord that can be nth'ed."
  [name args & body]
  (let [indexed-args (interleave (iterate inc 0) args)]
    `(defrecord ~name ~args
       clojure.lang.Indexed
       (nth [_# i#]
         (case i#
           ~@indexed-args
           (throw (IndexOutOfBoundsException.))))
       (nth [_# i# default#]
         (case i#
           ~@indexed-args
           default#))
       ~@body)))


; The Renderable protocol should be implemented by anything that can draw
; itself on the screen.
(defprotocol Renderable
  (render [this]))


(defn clamp-lower [n lower]
  (max n lower))

(defn clamp-upper [n upper]
  (min n upper))

(defn clamp [n lower upper]
  (-> n
    (clamp-lower lower)
    (clamp-upper upper)))


; lol Clojure
(defn sq [n]
  (* n n))

(defn sqrt [n]
  (Math/sqrt n))

(defn mean
  "Return the mean of the supplied sequence.

  You can pass the length of the sequence if you want.  This can save time if
  you want to precalculate it and use it for other things too.

  "
  ([s]
   (mean s (count s)))
  ([s length]
   (/ (reduce + s)
      length)))

(defn standard-deviation [s]
  (let [n (count s)
        average (mean s n)
        variance #(sq (- % average))]
    (sqrt (mean (map variance s) n))))

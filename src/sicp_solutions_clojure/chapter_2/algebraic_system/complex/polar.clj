(ns sicp-solutions-clojure.chapter-2.algebraic-system.complex.polar
  (:refer-clojure)
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system )))

(defn make [r a]
  (vector r a))

(def magnitude first)
(def angle second)

(defn real-part [z]
  (* (magnitude z) (Math/cos (angle z))))
(defn imag-part [z]
  (* (magnitude z) (Math/sin (angle z))))

(defn tag [x]
  (vector 'polar x))

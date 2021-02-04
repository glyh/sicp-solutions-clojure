(ns sicp-solutions-clojure.chapter-2.algebraic-system.complex.cartesian
  (:refer-clojure)
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system )))

(defn make [x y]
  (vector x y))

(def real-part first)
(def imag-part second)
(defn magnitude [x] 
  (Math/sqrt (+ (* (real-part x) (real-part x))
                (* (imag-part x) (imag-part x)))))
(defn angle [x] 
  (let [a (real-part x)
        b (imag-part x)]
    (cond 
      (not= 0 b) (* 2 (Math/atan (/ b (+ (magnitude x) a)))))
      (neg? a)   Math/PI
      :else      0))

(defn neg [x]
  (make (- (real-part x)) (imag-part x)))

(defn tag [x]
  (vector 'cartesian x))

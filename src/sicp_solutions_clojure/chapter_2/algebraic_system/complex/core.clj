(ns sicp-solutions-clojure.chapter-2.algebraic-system.complex.core
  (:refer-clojure :exclude [get type val zero?])
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system.complex
             [cartesian :as cartesian]
             [polar :as polar])))

(def ops-types-map {})

(defn get [symbols]
  (clojure.core/get ops-types-map symbols))

(defn put! [symbols val]
  (alter-var-root (var ops-types-map) 
                  (constantly (assoc ops-types-map symbols val))))

(defn type [x]
  (first x))

(defn apply-general [sym x] 
  ((get [sym (type x)]) x))

(def eps 1e-7)
(defn real-part [x] (apply-general 'real-part x))
(defn imag-part [x] (apply-general 'imag-part x))
(defn magnitude [x] (apply-general 'magnitude x))
(defn angle [x] (apply-general 'angle x))

(defn real-imag->complex [x y]
  ((get ['make 'cartesian]) x y))

(defn mag-ang->complex [r a]
  ((get ['make 'polar]) r a))

(defn add [z1 z2]
  (real-imag->complex (+ (real-part z1) (real-part z2))
                      (+ (imag-part z1) (imag-part z2))))
(defn sub [z1 z2]
  (real-imag->complex (- (real-part z1) (real-part z2))
                      (- (imag-part z1) (imag-part z2))))
(defn mul [z1 z2]
  (mag-ang->complex (* (magnitude z1) (magnitude z2))
                    (+ (angle z1) (angle z2))))

(defn div [z1 z2]
  (mag-ang->complex (/ (magnitude z1) (magnitude z2))
                    (- (angle z1) (angle z2))))

(defn eq? [z1 z2] (< (magnitude (sub z1 z2)) eps))
(defn zero? [z] (= (magnitude z) 0))


(defn tag [x] (vector 'complex x))

(def val second)

(defn init! []
  ; Polar representation
  (put! ['make 'polar] (comp polar/tag polar/make))
  (put! ['real-part 'polar] (comp polar/real-part val))
  (put! ['imag-part 'polar] (comp polar/imag-part val))
  (put! ['magnitude 'polar] (comp polar/magnitude val))
  (put! ['angle 'polar] (comp polar/angle val))
  ; Cartesian representation
  (put! ['make 'cartesian] (comp cartesian/tag cartesian/make))
  (put! ['real-part 'cartesian] (comp cartesian/real-part val))
  (put! ['imag-part 'cartesian] (comp cartesian/imag-part val))
  (put! ['magnitude 'cartesian] (comp cartesian/magnitude val))
  (put! ['angle 'cartesian] (comp cartesian/angle val)))

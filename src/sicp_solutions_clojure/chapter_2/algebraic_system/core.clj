(ns sicp-solutions-clojure.chapter-2.algebraic-system.core 
  (:refer-clojure :exclude [test zero?])
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system 
             [base :as base :refer [add mul div make eq? lower project zero?]]
             [polynomial :as polynomial]
             [ratio :as ratio])
            (sicp-solutions-clojure.chapter-2.algebraic-system.complex 
             [core :as complex])))

(defn init! [] 
  ; This piece of code is really ugly, I write them just to follow SICP. 
  ; ----------------------------------------------------------------------------
  ; Install primitive module 
    ; Operators:
  (base/put! ['make 'primitive] identity)
  (base/put! ['add 'primitive 'primitive] +)
  (base/put! ['negative 'primitive] -)
  (base/put! ['mul 'primitive 'primitive] *)
  (base/put! ['div 'primitive 'primitive] /)
    ; Predicates(complex/ma):
  (base/put! ['eq? 'primitive 'primitive] =)
  (base/put! ['zero? 'primitive] clojure.core/zero?)
    ; hierarchy
  (base/put! ['level 'primitive] 2)
  (base/put! ['raise 'primitive] 
             ; Raise operations doesn't know where to raise, 
             ; so there should be tags
             #(complex/tag (complex/real-imag->complex %1 0)))
  (base/put! ['project 'primitive] 
             (fn [x]
               (cond (int? x)   (ratio/make x 1)
                     (ratio? x) (ratio/make (numerator x) (denominator x))
                     :else      (ratio/make (* 1000000000N x) 1000000000N))))
  (base/put! ['project-to 'primitive] 'rational)
  ; Install ratio module 
    ; Operators:
  (base/put! ['make 'rational] ratio/make)
  (base/put! ['add 'rational 'rational] ratio/add)
  (base/put! ['neg 'rational ] ratio/neg)
  (base/put! ['mul 'rational 'rational] ratio/mul)
  (base/put! ['div 'rational 'rational] ratio/div)
    ; Predicates:
  (base/put! ['eq? 'rational 'rational] ratio/eq?)
  (base/put! ['zero? 'rational] ratio/zero?)
    ; Hierarchy:
  (base/put! ['level 'rational] 1)
  (base/put! ['raise 'rational] #(/ (ratio/numer %) (ratio/denom %)))
  ; Install complex module 
  (complex/init!)
    ; Operators:
  (base/put! ['make 'complex 'real-imag] complex/real-imag->complex)
  (base/put! ['make 'complex 'mag-ang] complex/mag-ang->complex)
  (base/put! ['add 'complex 'complex] complex/add)
  (base/put! ['neg 'complex] complex/neg)
  (base/put! ['mul 'complex 'complex] complex/mul)
  (base/put! ['div 'complex 'complex] complex/div)
    ; Predicates:
  (base/put! ['eq? 'complex 'complex] complex/eq?)
  (base/put! ['zero? 'complex] complex/zero?)
    ; Hierarchy:
  (base/put! ['level 'complex] 3)
  (base/put! ['project 'complex] complex/real-part)
  (base/put! ['project-to 'complex] 'primitive)
  ; Install coercion module
  ; (base/put-coercion! ['primitive 'complex] coercion/primitive->complex)
  ; Install polynomial module
    ; Operators:
  (base/put! ['make 'polynomial] polynomial/make-poly)
  (base/put! ['add 'polynomial 'polynomial] polynomial/add-poly)
  (base/put! ['mul 'polynomial 'polynomial] polynomial/mul-poly)
    ; Predicates:
  (base/put! ['zero? 'polynomial] polynomial/zero?)
    ; Hierarchy:
  (base/put! ['level 'polynomial] 4)
  (base/put! ['project 'polynomial] polynomial/find-constant)
  (base/put! ['project-to 'polynomial] 'complex))

(defn real-part [x] 
  (if (= (base/type x) 'complex)
    (complex/real-part (base/val x))
    (throw (Exception. (str "real-part: Expected `complex`, got: " (type x))))))
(defn imag-part [x] 
  (if (= (base/type x) 'complex)
    (complex/imag-part (base/val x))
    (throw (Exception. (str "imag-part: Expected `complex`, got: " (type x))))))
(defn magnitude [x] 
  (if (= (base/type x) 'complex)
    (complex/magnitude (base/val x))
    (throw (Exception. (str "magnitude: Expected `complex`, got: " (type x))))))
(defn angle [x] 
  (if (= (base/type x) 'complex)
    (complex/angle (base/val x))
    (throw (Exception. (str "angle: Expected `complex`, got: " (type x))))))

(defn test []
  (init!)
  (println (add (make ['primitive] 123) (make ['primitive] 456)))
  (println (mul (make ['rational] 3 4) (make ['rational] 12 24)))
  (println (div (make ['complex 'real-imag] 1 0) 
                (make ['complex 'mag-ang] 5 (/ Math/PI 3))))
  (println (eq? (make ['complex 'real-imag] 5/2 (* 5/2 (Math/sqrt 3))) 
                (make ['complex 'mag-ang] 5 (/ Math/PI 3))))
  (println (zero? (make ['rational] 0 3)))
  (println (add (make ['rational] 7 3) (make ['complex 'real-imag] 4 5)))
  (println (lower (make ['complex 'real-imag] 1 0))))

(comment (test))

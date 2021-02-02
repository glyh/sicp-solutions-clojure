(ns sicp-solutions-clojure.chapter-2.algebraic-system.core 
  (:refer-clojure :exclude [test zero?])
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system 
             [base :as base :refer [add sub mul div make eq? zero?]]
             [coercion :as coercion]
             [primitive :as primitive]
             [ratio :as ratio])
            (sicp-solutions-clojure.chapter-2.algebraic-system.complex 
             [core :as complex])))
(defn init! []
  ; Install primitive module 
    ; Operators:
  (base/put! ['make 'primitive] identity)
  (base/put! ['add 'primitive 'primitive] +)
  (base/put! ['sub 'primitive 'primitive] -)
  (base/put! ['mul 'primitive 'primitive] *)
  (base/put! ['div 'primitive 'primitive] /)
    ; Predicates:
  (base/put! ['eq? 'primitive 'primitive] =)
  (base/put! ['zero? 'primitive] clojure.core/zero?)
  ; Install ratio module 
    ; Operators:
  (base/put! ['make 'rational] #(ratio/tag (ratio/make %1 %2)))
  (base/put! ['add 'rational 'rational] #(ratio/tag (ratio/add %1 %2)))
  (base/put! ['sub 'rational 'rational] #(ratio/tag (ratio/sub %1 %2)))
  (base/put! ['mul 'rational 'rational] #(ratio/tag (ratio/mul %1 %2)))
  (base/put! ['div 'rational 'rational] #(ratio/tag (ratio/div %1 %2)))
    ; Predicates:
  (base/put! ['eq? 'rational 'rational] ratio/eq?)
  (base/put! ['zero? 'rational] ratio/zero?)
  ; Install complex module 
  (complex/init!)
    ; Operators:
  (base/put! ['make 'complex 'real-imag] 
             #(complex/tag (complex/real-imag->complex %1 %2)))
  (base/put! ['make 'complex 'mag-ang] 
             #(complex/tag (complex/mag-ang->complex %1 %2)))
  (base/put! ['add 'complex 'complex] #(complex/tag (complex/add %1 %2)))
  (base/put! ['sub 'complex 'complex] #(complex/tag (complex/sub %1 %2)))
  (base/put! ['mul 'complex 'complex] #(complex/tag (complex/mul %1 %2)))
  (base/put! ['div 'complex 'complex] #(complex/tag (complex/div %1 %2)))
    ; Predicates:
  (base/put! ['eq? 'complex 'complex] complex/eq?)
  (base/put! ['zero? 'complex] complex/zero?)
  ; Install coercion module
  (base/put-coercion! ['primitive 'complex] coercion/primitive->complex))



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
  (println (zero? (make ['rational] 0 3))))
(comment (test))

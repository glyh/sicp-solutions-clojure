;; (use 'clojure.tools.trace)
(ns sicp-solutions-clojure.chapter-2.algebraic-system.core
  (:refer-clojure)
  (:require
    [sicp-solutions-clojure.chapter-2.algebraic-system.base :as base
     :refer [add sub mul div make eq? lower alg-zero? remainder]]
    [sicp-solutions-clojure.chapter-2.algebraic-system.complex.core :as complex]
    [sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.core :as poly]
    [sicp-solutions-clojure.chapter-2.algebraic-system.ratio :as ratio]
    [sicp-solutions-clojure.chapter-2.algebraic-system.ratio-function
     :as ratio-func]))


(defn init!
  []
  ; This piece of code is really ugly (It's not functional), I write them just to follow SICP.
  ; ----------------------------------------------------------------------------
  ; Install primitive module
  ; Operators:
  (base/put! ['make 'primitive] identity)
  (base/put! ['add 'primitive 'primitive] +)
  (base/put! ['neg 'primitive] -)
  (base/put! ['mul 'primitive 'primitive] *)
  (base/put! ['div 'primitive 'primitive] /)
  ; Predicates:
  (let [eps 1e-7
        abs #(if (< % 0) (- %) %)]
    (base/put! ['eq? 'primitive 'primitive] #(< (abs (- %1 %2)) eps)))
  (base/put! ['zero? 'primitive] clojure.core/zero?)
  ; hierarchy
  (base/put! ['level 'primitive] 2)
  (base/put! ['raise 'primitive]
             ; Raise operations doesn't know where to raise,
             ; so there should be tags
             #(complex/tag (complex/real-imag->complex % 0)))
  (base/put! ['project 'primitive]
             (fn [x]
               (cond (int? x) (ratio/make x 1)
                     (ratio? x) (ratio/make (numerator x) (denominator x))
                     :else (ratio/make (bigint (* 1000000000N x)) 1000000000N))))
  (base/put! ['project-to 'primitive] 'rational)
  ; Install ratio module
  ; Operators:
  (base/put! ['make 'rational] ratio/make)
  (base/put! ['add 'rational 'rational] ratio/add)
  (base/put! ['neg 'rational] ratio/neg)
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
  (base/put! ['raise 'complex]
             #(poly/tag (poly/make-poly '_ (vector 'dense [['complex %]]))))
  (base/put! ['project 'complex] complex/real-part)
  (base/put! ['project-to 'complex] 'primitive)
  ; Install coercion module
  ; (base/put-coercion! ['primitive 'complex] coercion/primitive->complex)
  ; Install polynomial module
  (poly/init!)
  ; Operators:
  (base/put! ['make 'polynomial] poly/make-poly)
  (base/put! ['add 'polynomial 'polynomial] poly/add-poly)
  (base/put! ['neg 'polynomial] poly/neg-poly)
  (base/put! ['mul 'polynomial 'polynomial] poly/mul-poly)
  (base/put! ['div 'polynomial 'polynomial] poly/div-poly)
  (base/put! ['rem 'polynomial 'polynomial] poly/rem-poly)
  ; Predicates:
  (base/put! ['eq? 'polynomial 'polynomial] poly/eq?-poly)
  (base/put! ['zero? 'polynomial] poly/zero?-poly)
  ; Hierarchy:
  (base/put! ['level 'polynomial] 4)
  (base/put! ['raise 'polynomial] #(poly/tag (poly/make-poly '_ (vector 'dense [['polynomial %]]))))
  (base/put! ['project 'polynomial] poly/constant-poly)
  ; Install rational funcion module
  ; Operators:
  (base/put! ['make 'ratio-func] ratio-func/make)
  (base/put! ['add 'ratio-func 'ratio-func] ratio-func/add)
  (base/put! ['neg 'ratio-func] ratio-func/neg)
  (base/put! ['mul 'ratio-func 'ratio-func] ratio-func/mul)
  (base/put! ['div 'ratio-func 'ratio-func] ratio-func/div)
  ; Predicates:
  (base/put! ['eq? 'ratio-func 'ratio-func] ratio-func/eq?)
  (base/put! ['zero? 'ratio-func] ratio-func/zero?)
  ; Hierarchy:
  (base/put! ['level 'ratio-func] 5)
  (base/put! ['project 'ratio-func] #(base/div (ratio-func/numer %) (ratio-func/denom %))))


(defn real-part
  [x]
  (if (= (base/type x) 'complex)
    (complex/real-part (base/val x))
    (throw (Exception. (str "real-part: Expected `complex`, got: " (type x))))))


(defn imag-part
  [x]
  (if (= (base/type x) 'complex)
    (complex/imag-part (base/val x))
    (throw (Exception. (str "imag-part: Expected `complex`, got: " (type x))))))


(defn magnitude
  [x]
  (if (= (base/type x) 'complex)
    (complex/magnitude (base/val x))
    (throw (Exception. (str "magnitude: Expected `complex`, got: " (type x))))))


(defn angle
  [x]
  (if (= (base/type x) 'complex)
    (complex/angle (base/val x))
    (throw (Exception. (str "angle: Expected `complex`, got: " (type x))))))


(defn alg-test
  []
  (init!)
  (println (add (make ['primitive] 123) (make ['primitive] 456)))
  (println (mul (make ['rational] 3 4) (make ['rational] 12 24)))
  (println (div (make ['complex 'real-imag] 1 0)
                (make ['complex 'mag-ang] 5 (/ Math/PI 3))))
  (println (eq? (make ['complex 'real-imag] 5/2 (* 5/2 (Math/sqrt 3)))
                (make ['complex 'mag-ang] 5 (/ Math/PI 3))))
  (println (alg-zero? (make ['rational] 0 3)))
  (println (add (make ['rational] 7 3) (make ['complex 'real-imag] 4 5)))
  (println (lower (make ['complex 'real-imag] 1 0)))
  (let [P1 (make ['polynomial] 'x (poly/vector->dense-terms [1]))
        P2 (make ['polynomial] 'x (poly/vector->dense-terms [1 2]))]
    (println (make ['ratio-func] P1 P2)))

  (let [P1 (make ['polynomial] 'x
                 (poly/list->sparse-terms '((0 3) (1 4) (4 9))))
        P2 (make ['polynomial] 'y
                 (poly/list->sparse-terms '((0 3) (1 4) (4 9))))
        P3 (make ['polynomial] 'x (poly/vector->dense-terms [1 4 2 8 5 7]))
        P4 (make ['polynomial] 'x (poly/vector->dense-terms [1 3 5]))
        ]
    ;;(println (mul P3 (sub P1 P2)));;Correct
    ;;(println (div P3 P4) (remainder P3 P4));;Correct
    ;;(println (add (mul P3 P2) (mul P1 P4)))
    (println (add (make ['ratio-func] P1 P3) (make ['ratio-func] P2 P4)))
    )


  (let [P1 (make ['polynomial] 'x (poly/vector->dense-terms [1 -2 1]))
        P2 (make ['polynomial] 'x (poly/vector->dense-terms [11 0 7]))
        P3 (make ['polynomial] 'x (poly/vector->dense-terms [13 5]))]
    (println (ratio-func/gcd (mul P1 P2) (mul P1 P3)))))


(comment (identity *e))
(comment (alg-test))

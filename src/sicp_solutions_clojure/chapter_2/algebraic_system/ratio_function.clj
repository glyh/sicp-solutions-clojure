(ns sicp-solutions-clojure.chapter-2.algebraic-system.ratio-function
  (:refer-clojure :exclude [zero?])
  (:require
    [sicp-solutions-clojure.chapter-2.algebraic-system.base :as base]
    [sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.core :as poly]
    ))


(def numer first)
(def denom second)


(defn prniden [x]
  (prn x)
  x)

(defn exp [x n]
  (reduce base/mul (repeat n x)))


(defn gcd-num [a b]
  (if (= b 0) a (recur b (mod a b))))


(defn gcd-num-reduce [t]
  ;;(prn "gcd-num-reduce:" t)
  (reduce gcd-num t))


(defn gcd
  [a b]
  ;;(prn "gcd " a b)
  (if (base/alg-zero? b)
    (base/div a
      (gcd-num-reduce (map (comp base/->primitive :coeff)
                           (second (poly/->sparse (:terms (base/val a)))))))
    (let [c2 (->> b
                  base/val
                  :terms
                  poly/->sparse
                  base/val
                  first
                  :coeff)
          o1 (->> a
                  base/val
                  :terms
                  poly/->sparse
                  base/val
                  first
                  :order)
          o2 (->> b
                  base/val
                  :terms
                  poly/->sparse
                  base/val
                  first
                  :order)]
      ;;(prn c2 o1 o2)
      (recur b (base/remainder (base/mul a (exp c2 (+ 1 o1 (- o2)))) b)))))


(defn make
  [n d]
  ;;(prn "make ratio-func:" n d)
  (assert (= (base/type n) 'polynomial))
  (assert (= (base/type d) 'polynomial))
  (assert (not (base/alg-zero? d)))
  (vector n d)) ;; gcd of polynomials might not exists.


(defn add
  [x y]
  ;; (prn 'ratio-func-add (numer x) (numer y) (denom x) (denom y))
  (make (base/add (base/mul (numer x) (denom y))
                  (base/mul (denom x) (numer y)))
        (base/mul (denom x) (denom y))))


(defn neg
  [x]
  (make (base/neg (numer x)) (denom x)))


(defn sub
  [x y]
  (make (base/sub (base/mul (numer x) (denom y))
                  (base/mul (denom x) (numer y)))
        (base/mul (denom x) (denom y))))


(defn mul
  [x y]
  (make (base/mul (numer x) (numer y)) (base/mul (denom x) (denom y))))


(defn div
  [x y]
  (make (base/mul (numer x) (denom y)) (base/mul (denom x) (denom y))))


(defn eq?
  [x y]
  (= (base/mul (numer x) (denom y))
     (base/mul (numer y) (denom x))))


(defn zero?
  [x]
  (base/alg-zero? (numer x)))


(defn tag
  [x]
  (vector 'ratio-func x))

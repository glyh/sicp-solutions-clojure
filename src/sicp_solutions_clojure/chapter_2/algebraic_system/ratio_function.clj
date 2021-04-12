(ns sicp-solutions-clojure.chapter-2.algebraic-system.ratio-function
  (:refer-clojure :exclude [zero?])
  (:require
    [sicp-solutions-clojure.chapter-2.algebraic-system.base :as base]))


(def numer first)
(def denom second)


(defn gcd
  [a b]
  (println "ratio-func/gcd " a b)
  (if (base/alg-zero? b)
    a
    (recur b (base/remainder a b))))


(defn make
  [n d]
  (prn "make ratio-func:" n d)
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

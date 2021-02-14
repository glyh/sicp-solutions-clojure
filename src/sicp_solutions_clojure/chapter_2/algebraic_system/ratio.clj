(ns sicp-solutions-clojure.chapter-2.algebraic-system.ratio
  (:refer-clojure :exclude [zero?]))

(defn gcd [x y]
  (loop [a x b y]
    (if (clojure.core/zero? b) 
      a
      (recur b (mod a b)))))

(def numer first)
(def denom second)

(defn sgn [x]
  (cond 
    (> x 0) 1
    (< x 0) -1
    :else   0))
(defn abs [x]
  (if (>= x 0) x (- x))) 

(defn make [n d]
  (assert (not= d 0))
  (let [g (* (sgn d) (abs (gcd n d)))]
    (vector (/ n g) (/ d g))))

(defn add [x y]
  (make (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(defn neg [x] 
  (make (- (numer x)) (denom x)))

(defn sub [x y]
  (make (- (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))
(defn mul [x y]
  (make (* (numer x) (numer y)) (* (denom x) (denom y))))
(defn div [x y]
  (make (* (numer x) (denom y)) (* (denom x) (denom y))))

(defn eq? [x y]
  (= (* (numer x) (denom y)) 
     (* (numer y) (denom x))))
(defn zero? [x] (= (numer x) 0))

(defn tag [x] (vector 'rational x))

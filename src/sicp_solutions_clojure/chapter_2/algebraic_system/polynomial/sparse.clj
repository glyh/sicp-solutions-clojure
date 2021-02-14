(ns sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.sparse
  (:refer-clojure :exclude [zero? first])
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system
             [base :as base])))

(def tag #(vector 'sparse %))

(defn make-term [order coeff] 
  (assert (and (int? order)
               (base/number? coeff)))
  {:order order,
   :coeff coeff})

(defn list->sparse-terms [l]
  (assert (= (type l) clojure.lang.PersistentList))
  (->> l
       (map (fn [x] 
              (assert (number? (clojure.core/first x)))
              {:order (clojure.core/first x) :coeff (second x)}))
       (sort (fn [x y] (> (:order x) (:order y))))
       tag))

(defn adjoin-term [term term-list] 
  (if (base/zero? (:coeff term))
    term-list 
    (conj term-list term )))

(defn add-terms [L1 L2]
  (cond 
    (empty? L1) L2
    (empty? L2) L1
    :else (let [t1 (clojure.core/first L1)
               t2 (clojure.core/first L2)]
            (cond 
              (> (:order t1) (:order t2)) 
                (adjoin-term t1 (add-terms (rest L1) L2))
              (< (:order t1) (:order t2))
                (adjoin-term t2 (add-terms L1 (rest L2)))
              :else 
                (adjoin-term 
                 (make-term (:order t1) (base/add (:coeff t1) (:coeff t2)))
                 (add-terms (rest L1) (rest L2)))))))

(defn mul-term-terms [t1 L2]
  (if (empty? L2)
    ()
    (let [t2 (clojure.core/first L2)]
      (adjoin-term (make-term (+ (:order t1) (:order t2))
                       (base/mul (:coeff t1) (:coeff t2)))
            (mul-term-terms t1 (rest L2))))))

(defn mul-terms [L1 L2]
  (if (empty? L1)
    ()
    (add-terms (mul-term-terms (clojure.core/first L1) L2) (mul-terms (rest L1) L2))))

(defn neg-terms [t]
  (map #(make-term (:order %) (base/neg (:coeff %))) t))

(defn constant [t]
  (cond 
    (empty? t)
      0
    (= (:order (clojure.core/first t)) 0)
      (:coeff (clojure.core/first t))
    :else 
      (recur (rest t))))

(defn add-constant [t c]
  (cond 
    (empty? t) 
      (make-term 0 c)
    (> (:order (clojure.core/first t) ) 0) 
      (concat (clojure.core/first t) (add-constant (rest t) c))
    :else 
      (list (base/add (clojure.core/first t) c))))

(defn constant? [t]
  (or (= (count t) 0) 
      (and (= (count t) 1) (= (:order (clojure.core/first t)) 0))))

(defn zero? [t] (every? (comp zero? :coeff) t))

(def first clojure.core/first)

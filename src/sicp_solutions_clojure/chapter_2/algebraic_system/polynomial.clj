(ns sicp-solutions-clojure.chapter-2.algebraic-system.polynomial
  (:refer-clojure :exclude [zero?])
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system
             [base :as base])))

(defn make-poly [v terms]
  (assert (and (= (type v) clojure.lang.Symbol)
               (= (type terms) clojure.lang.PersistentList)))
  {:variable v, 
   :terms    terms})

(defn make-term [order coeff] 
  (assert (and (int? order)
               (base/number? coeff)))
  {:order order,
   :coeff coeff})

(defn adjoin-term [term term-list] 
  (if (base/zero? (:coeff term))
    term-list 
    (conj term term-list)))

(defn add-terms [L1 L2]
  (cond 
    (empty? L1) L2
    (empty? L2) L1
    :else (let [t1 (first L1)
               t2 (first L2)]
            (cond 
              (> (:order t1) (:order t2)) 
                (adjoin-term t1 (add-terms (rest L1) L2))
              (< (:order t1) (:order t2))
                (adjoin-term t2 (add-terms L1 (rest L2)))
              :else 
                (adjoin-term 
                 (make-term (:order t1) (base/add (:coeff t1) (:coeff t2)))
                 (add-terms (rest L1) (rest L2)))))))

(defn add-poly [p1 p2]
  (if (= (:variable p1) (:variable p2)) 
    ; This is incomplete, what about constant?
    (make-poly (:variable p1) (add-terms (:terms p1) (:terms p2)))
    (throw (Exception. (str "Polys not in same var: ADD-POLY" (list p1 p2))))))

(defn mul-term-terms [t1 L2]
  (if (empty? L2)
    ()
    (let [t2 (first L2)]
      (adjoin-term (make-term (+ (:order t1) (:order t2))
                       (base/mul (:coeff t1) (:coeff t2)))
            (mul-term-terms t1 (rest L2))))))

(defn mul-terms [L1 L2]
  (if (empty? L1)
    ()
    (add-terms (mul-term-terms (first L1) L2) (mul-terms (rest L1) L2))))

(defn mul-poly [p1 p2]
  (if (= (:variable p1) (:variable p2))
    (make-poly (:variable p1) (mul-terms (:terms p1) (:terms p2)))
    (throw (Exception. (str "Polys not in same var: MUL-POLY" (list p1 p2))))))

(defn zero? [p] 
  (empty? (:terms p)))

(defn neg [p]
  (make-poly (:variable p) 
             (map #(make-term (:order %) (base/neg (:coeff %))) 
                  (:terms p))))

(def tag #(vector 'polynomial %))

(defn find-constant [p]
  ((fn [terms]
     (cond 
       (empty? terms)
         0
       (= (:order (first terms)) 0)
         (:coeff (first terms))
       :else 
         (recur (rest terms)))) 
   (:terms p)))

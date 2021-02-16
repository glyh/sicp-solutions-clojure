(ns sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.sparse
  (:refer-clojure :exclude [zero? first])
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system
             [base :as base])))

(def tag #(vector 'sparse %))

(defn make-term [order coeff] 
  ;; (prn order coeff)
  (assert (and (int? order)
               (base/number? coeff)))
  {:order order,
   :coeff coeff})

(defn list->sparse-terms [l]
  (assert (= (type l) clojure.lang.PersistentList))
  (->> l
       (map (fn [x] 
              (assert (base/number? (clojure.core/first x)))
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
                (let [c-new  (base/add (:coeff t1) (:coeff t2))
                      t-rest (add-terms (rest L1) (rest L2))]
                  (if (base/zero? c-new) 
                    t-rest 
                    (adjoin-term (make-term (:order t1) c-new) t-rest)))))))

(defn mul-term-terms [t1 L2]
  ;; (prn t1 L2)
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
  (let [x (clojure.core/first t)]
    (cond 
      (empty? t)
        (list (make-term 0 c))
      (> (:order x) 0)
        (conj (add-constant (rest t) c) x)
      :else 
        (let [new-c (base/add (:coeff x) ['polynomial c])] 
          (if (base/zero? new-c) () (list (make-term 0 new-c)))))))

(defn constant? [t]
  (or (= (count t) 0) 
      (and (= (count t) 1) (= (:order (clojure.core/first t)) 0))))

(defn zero? [t] 
  (every? (comp base/zero? :coeff) t))

(defn strip-zeros [t]
  (cond 
    (empty? t) 
      ()
    (base/zero? (:coeff (clojure.core/first t))) 
      (strip-zeros (rest t))
    :else 
      (conj (strip-zeros (rest t)) (clojure.core/first t))))

(def first clojure.core/first)

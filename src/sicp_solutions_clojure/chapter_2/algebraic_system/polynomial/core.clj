(ns sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.core
  (:refer-clojure :exclude [zero? get type val first])
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system
             [base :as base]))
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system.polynomial
             [sparse :as sparse]
             [dense :as dense])))

(def ops-types-map {})

(defn get [symbols]
  (clojure.core/get ops-types-map symbols))

(defn put! [symbols val]
  (alter-var-root (var ops-types-map) 
                  (constantly (assoc ops-types-map symbols val))))

(def type clojure.core/first)
(def val second)

(defn ->sparse [terms]
  (if (= (type terms) 'sparse) terms ;;dense
    (let [n (count (val terms))
          f (fn [idx itm]
              (sparse/make-term (- n idx 1) itm))]
      (->> (val terms)
           (map-indexed f)
           (vector 'sparse)))))

(declare constant)
(declare constant?)

(defn strip-vars [p]
  (loop [r p]
    (if (constant? r)
      (recur (constant r))
      r)))

(defn apply-general [sym & args] 
  ;; (prn sym (map ->sparse args))
  ; Till now my codes are highly inconsistent. since haven't thought of the 
  ; design clearly before. Also, the requirements (from the exercises) keep 
  ; changing. 

  ; Here I choose all coerce to sparse one, though it might not be a great way. 
  (if (and (some #(= 'dense (type %)) args) (some #(= 'sparse (type %)) args))
    (recur sym (map ->sparse args))
    ;; (vector 'sparse (apply (get [sym 'sparse]) (map ->sparse args)))
    (let [t      (type (clojure.core/first args))
          result (apply (get [sym t]) (map val args))] 
      (cond 
        (boolean? result) result
        (= sym 'constant) (vector t result) ;; Avoid dead loop
        :else             (strip-vars (vector (t result)))))))

(def list->sparse-terms sparse/list->sparse-terms)
(def vector->dense-terms dense/vector->dense-terms)

(defn make-poly [v terms]
  {:variable v, 
   :terms    terms})

(defn add-terms [t1 t2]
  (apply-general 'add t1 t2))
(defn neg-term [t]
  (apply-general 'neg t))
(defn sub-terms [t1 t2]
  (add-terms t1 (neg-term t2)))
(defn mul-terms [t1 t2]
  (apply-general 'mul t1 t2))

(defn neg-poly [p]
  (make-poly (:variable p) (neg-term (:terms p))))

(defn constant [p]
  (apply-general 'constant (:terms p)))

(defn constant? [p]
  (apply-general 'constant? (:terms p)))

(defn add-constant [p c] 
  (apply-general 'add-constant (:terms p) c))

(defn add-poly [p1 p2]
  (prn p1 p2)
  (cond 
    (or (= (:variable p1) (:variable p2))
        (constant? (:terms p2)))
      (make-poly (:variable p1) (add-terms (:terms p1) (:terms p2)))
    (constant? (:terms p1))
      (make-poly (:variable p2) (add-terms (:terms p1) (:terms p2)))
    (< (compare (:variable p1) (:variable p2)) 0)
      (add-constant p1 p2)
    :else 
      (add-constant p2 p1)))

(defn ->constant [t]
  (vector->dense-terms [t]))

(defn mul-poly [p1 p2]
  (cond 
    (or (= (:variable p1) (:variable p2))
        (constant? (:terms p2)))
      (make-poly (:variable p1) (mul-terms (:terms p1) (:terms p2)))
    (constant? (:terms p1))
      (make-poly (:variable p2) (mul-terms (:terms p2) (:terms p1)))
    (< (compare (:variable p1) (:variable p2)) 0)
      (make-poly (:variable p1) 
                 (mul-terms (:terms p1) (->constant (:terms p2))))
    :else 
      (make-poly (:variable p2) 
                 (mul-terms (:terms p2) (->constant (:terms p1))))))

(defn zero? [p] 
  (apply-general 'zero? (:terms p)))

(defn first [l]
  ((get ['first (type l)]) (val l)))

(def terms-nil (vector->dense-terms []))

(defn div-terms [l1 l2] 
  (if (zero? l1)
    (list terms-nil terms-nil)
    (let [t1 (first l1)
          t2 (first l2)]
      (if (> (:order t2) (:order t1)) 
        (list terms-nil l1)
        (let [major-term (list->sparse-terms 
                           (list (list (base/div (:coeff t1) (:coeff t2)) 
                                       (- (:order t1) (:order t2)))))
              [q r] (div-terms 
                      (sub-terms l1 (mul-terms l2 major-term))
                      l2)]
          (list (add-terms q major-term) r))))))

(def tag #(vector 'polynomial %))

(defn init![]
  (put! ['zero? 'dense] dense/zero?)
  (put! ['zero? 'sparse] sparse/zero?)
  (put! ['constant? 'dense] dense/constant?)
  (put! ['constant? 'sparse] sparse/constant?)
  (put! ['constant 'dense] dense/constant)
  (put! ['constant 'sparse] sparse/constant)
  (put! ['add-constant 'sparse] sparse/add-constant)
  (put! ['add-constant 'dense] dense/add-constant)
  (put! ['add 'dense] dense/add-terms)
  (put! ['add 'sparse] sparse/add-terms)
  (put! ['mul 'dense] dense/mul-terms)
  (put! ['mul 'sparse] sparse/mul-terms)
  (put! ['neg 'dense] dense/neg-terms)
  (put! ['neg 'sparse] sparse/neg-terms)
  (put! ['first 'dense] dense/first)
  (put! ['first 'sparse] sparse/first))

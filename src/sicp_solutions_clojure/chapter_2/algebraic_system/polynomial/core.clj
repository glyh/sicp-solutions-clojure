(ns sicp-solutions-clojure.chapter-2.algebraic-system.polynomial
  (:refer-clojure :exclude [zero? get type])
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system)
            (sicp-solutions-clojure.chapter-2.algebraic-system.polynomial
             [sparse :as sparse]
             [dense :as dense])))

(def ops-types-map {})

(defn get [symbols]
  (clojure.core/get ops-types-map symbols))

(defn put! [symbols val]
  (alter-var-root (var ops-types-map) 
                  (constantly (assoc ops-types-map symbols val))))

(defn type [x]
  (first x))

(defn dense->sparse [terms]
  terms)
(defn apply-general [sym & args] 
  ; Till now my codes are highly inconsistent. since haven't thought of the 
  ; design clearly before. Also, the requirements (from the exercises) keep 
  ; changing . 

  ; Here I choose all coerce to sparse one, though it might not be a great way. 
  (let [x (first args)] (apply (get [sym (type x)]) args)))

(defn make-poly [v terms]
  (assert (and (= (type v) clojure.lang.Symbol)
               (seq? terms)))
  {:variable v, 
   :terms    terms})

(defn add-terms [t1 t2]
  (apply-general 'add t1 t2))
(defn mul-terms [t1 t2]
  (apply-general 'mul t1 t2))
(defn neg [p]
  (apply-general 'neg p))
(defn find-constant [p]
  (apply-general 'find-constant p))

(defn add-poly [p1 p2]
  (if (= (:variable p1) (:variable p2)) 
    ; This is incomplete, what about constant?
    (make-poly (:variable p1) (add-terms (:terms p1) (:terms p2)))
    (throw (Exception. (str "Polys not in same var: ADD-POLY" (list p1 p2))))))

(defn mul-poly [p1 p2]
  (if (= (:variable p1) (:variable p2))
    (make-poly (:variable p1) (mul-terms (:terms p1) (:terms p2)))
    (throw (Exception. (str "Polys not in same var: MUL-POLY" (list p1 p2))))))

(defn zero? [p] 
  (empty? (:terms p)))

(def tag #(vector 'polynomial %))

(ns sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.core
  (:refer-clojure :exclude [zero? get type val])
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system.polynomial
             [sparse :as sparse]
             [dense :as dense])))

(def ops-types-map {})

(defn get [symbols]
  (clojure.core/get ops-types-map symbols))

(defn put! [symbols val]
  (alter-var-root (var ops-types-map) 
                  (constantly (assoc ops-types-map symbols val))))

(def type first)
(def val second)

(defn ->sparse [terms]
  (if (= (type terms) 'sparse) terms ;;dense
    (let [n (count (val terms))
          f (fn [idx itm]
              (sparse/make-term (- n idx 1) itm))]
      (->> (val terms)
           (map-indexed f)
           (vector 'sparse)))))

(defn apply-general [sym & args] 
  ;; (prn sym (map ->sparse args))
  ; Till now my codes are highly inconsistent. since haven't thought of the 
  ; design clearly before. Also, the requirements (from the exercises) keep 
  ; changing. 

  ; Here I choose all coerce to sparse one, though it might not be a great way. 
  (if (and (some #(= 'dense (type %)) args) (some #(= 'sparse (type %)) args))
    (recur sym (map ->sparse args))
    ;; (vector 'sparse (apply (get [sym 'sparse]) (map ->sparse args)))
    (let [x (first args)] 
      (vector (type x) (apply (get [sym (type x)]) (map val args))))))

(def list->sparse-terms sparse/list->sparse-terms)
(def vector->dense-terms dense/vector->dense-terms)

(defn make-poly [v terms]
  {:variable v, 
   :terms    terms})

(defn add-terms [t1 t2]
  (apply-general 'add t1 t2))
(defn mul-terms [t1 t2]
  (apply-general 'mul t1 t2))
(defn neg [p]
  (make-poly (:variable p) (apply-general 'neg (:terms p))))
(defn find-constant [p]
  (apply-general 'find-constant (:terms p)))

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
  (apply-general 'zero? (:terms p)))

(def tag #(vector 'polynomial %))

(defn init![]
  (put! ['zero? 'dense] dense/zero?)
  (put! ['zero? 'sparse] sparse/zero?)
  (put! ['find-constant 'dense] dense/find-constant)
  (put! ['find-constant 'sparse] sparse/find-constant)
  (put! ['add 'dense] dense/add-terms)
  (put! ['add 'sparse] sparse/add-terms)
  (put! ['mul 'dense] dense/mul-terms)
  (put! ['mul 'sparse] sparse/mul-terms)
  (put! ['neg 'dense] dense/neg-terms)
  (put! ['neg 'sparse] sparse/neg-terms))

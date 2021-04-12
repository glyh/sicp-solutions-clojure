(ns sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.core
  (:refer-clojure :exclude [zero? get type val first])
  (:require
    [sicp-solutions-clojure.chapter-2.algebraic-system.base :as base]
    [sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.dense :as dense]
    [sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.sparse :as sparse]))


(def ops-types-map {})


(defn get
  [symbols]
  (clojure.core/get ops-types-map symbols))


(defn put!
  [symbols val]
  (alter-var-root (var ops-types-map)
                  (constantly (assoc ops-types-map symbols val))))


(defn ->sparse
  [terms]
  (if (= (base/type terms) 'dense)
    (let [n (count (base/val terms))
          f (fn [idx itm]
              (sparse/make-term (- n idx 1) itm))]
      (->> (base/val terms)
           (map-indexed f)
           (vector 'sparse)))
    terms))


;; stripping redundent vars is not possible in apply-general.

(defn apply-general
  [sym & args]
  ;; Design fault here: apply-general needs to know the variable!!!!
  ;; Or else, it can't judge whether a polynomial is constant or not!

  ;; result in terms
  ; Till now my codes are highly inconsistent. since haven't thought of the
  ; design clearly before. Also, the requirements (from the exercises) keep
  ; changing.

  ; Here I choose all coerce to sparse one, though it might not be a great way.
  ;; (prn "poly-apply-general" sym args)
  (if (and (some #(= 'dense (base/type %)) args)
           (some #(= 'sparse (base/type %)) args))
    (recur sym (map ->sparse args))
    ;; (vector 'sparse (apply (get [sym 'sparse]) (map ->sparse args)))
    (let [t      (base/type (clojure.core/first args))
          result (apply (get [sym t]) (map base/val args))]
      (cond
        (boolean? result) result
        (= sym 'constant) result ;; Avoid dead loop
        :else             (vector t result)))))


(def list->sparse-terms sparse/list->sparse-terms)
(def vector->dense-terms dense/vector->dense-terms)


(defn make-poly
  [v terms]
  {:variable v,
   :terms    terms})


(defn add-terms
  [t1 t2]
  ;; (prn "add terms " t1 t2)
  (apply-general 'add t1 t2))


(defn neg-term
  [t]
  (apply-general 'neg t))


(defn sub-terms
  [t1 t2]
  (add-terms t1 (neg-term t2)))


(defn mul-terms
  [t1 t2]
  ;; (prn 'poly/mul-terms t1 t2)
  (apply-general 'mul t1 t2))


(defn neg-poly
  [p]
  (make-poly (:variable p) (neg-term (:terms p))))


(defn constant-term
  [t]
  (apply-general 'constant t))


(defn constant-poly
  [p]
  ;; (prn "constant-poly: " p)
  (constant-term (:terms p)))


(defn constant?-term
  [t]
  (apply-general 'constant? t))


(defn constant?-poly
  [p]
  (constant?-term (:terms p)))


(def tag #(vector 'polynomial %))


(defn add-constant-poly
  [p c]
;; (prn "add-constant-poly" p c)
  (make-poly (:variable p) (apply-general 'add-constant (:terms p) c)))


(defn add-poly
  [p1 p2]
  (cond
    (or (= (:variable p1) (:variable p2))
        (constant?-poly p2))
    (make-poly (:variable p1) (add-terms (:terms p1) (:terms p2)))
    (constant?-poly p1)
    (make-poly (:variable p2) (add-terms (:terms p1) (:terms p2)))
    (< (compare (:variable p1) (:variable p2)) 0)
    (add-constant-poly p1 (tag p2))
    :else
    (add-constant-poly p2 (tag p1))))


(defn ->constant-term
  [t]
  (vector->dense-terms [['polynomial t]]))


(defn mul-poly
  [p1 p2]
  ;; (prn 'poly/mul-poly p1 p2)
  (cond
    (or (= (:variable p1) (:variable p2))
        (constant?-poly p2))
    (make-poly (:variable p1) (mul-terms (:terms p1) (:terms p2)))
    (constant?-poly p1)
    (make-poly (:variable p2) (mul-terms (:terms p2) (:terms p1)))
    (< (compare (:variable p1) (:variable p2)) 0)
    (make-poly (:variable p1)
               (mul-terms (:terms p1) (->constant-term p2)))
    :else
    (make-poly (:variable p2)
               (mul-terms (:terms p2) (->constant-term p1)))))


(defn zero?
  [t]
  ;; (prn 'poly/zero? t)
  (apply-general 'zero? t))


(defn zero?-poly
  [p]
  ;; (prn 'poly/zero?-poly p)
  (zero? (:terms p)))


(defn strip-zeros
  [t]
  (apply-general 'strip-zeros t))


(defn eq?
  [t1 t2]
  ;; (prn 'polynomial-eq? t1 t2)
  (if (not= (base/type t1) (base/type t2))
    (recur (->sparse t1) (->sparse t2))
    (let [st1 (second (strip-zeros t1))
          st2 (second (strip-zeros t2))]
      ;; (prn "striped:" st1 st2)
      (and (= (count st1) (count st2))
           (->> (map (fn [t1 t2]
                       (base/eq? (:coeff t1) (:coeff t2))) st1 st2)
                (every? true?))))))


(defn strip-vars-poly
  [p]
  (cond
    (constant?-poly p) (= (base/type (constant-poly p)) 'polynomial))
  (if (and (constant?-poly p) (= (base/type (constant-poly p)) 'polynomial))
    (strip-vars-poly (constant-poly p))))


(defn eq?-poly
  [p1 p2]
  (and
    (or
      (= (:variable p1) '_)
      (= (:variable p2) '_)
      (= (:variable p1) (:variable p2)))
    (eq? (:terms p1) (:terms p2))))


(defn first
  [l]
  ((get ['first (base/type l)]) (base/val l)))


(def terms-nil (vector->dense-terms []))

; Dividing is seperate to 3 parts, this is because apply-general have types
; to identify, and just can't identify a tuple of 2 polynomials.
(defn div-terms
  [l1 l2]
  ;;(prn "div terms" l1 l2)
  (if (zero? l1)
    terms-nil
    (let [t1 (first l1)
          t2 (first l2)]
      (if (> (:order t2) (:order t1))
        terms-nil
        (do
;;(prn "get major term:" (list->sparse-terms
;;                           (list (list (- (:order t1) (:order t2))
;;                                       (base/div (:coeff t1) (:coeff t2))))))
        (let [major-term (list->sparse-terms
                           (list (list (- (:order t1) (:order t2))
                                       (base/div (:coeff t1) (:coeff t2)))))
              q          (div-terms
                           (sub-terms l1 (mul-terms l2 major-term))
                           l2)]
          (add-terms q major-term))
          )
        ))))


(defn div-poly
  [p1 p2]
  (cond
    (or (= (:variable p1) (:variable p2))
        (constant?-poly p2))
    (make-poly (:variable p1) (div-terms (:terms p1) (:terms p2)))
    (constant?-poly p1)
    (make-poly (:variable p2) (div-terms (:terms p1) (:terms p2)))
    (< (compare (:variable p1) (:variable p2)) 0)
    (make-poly (:variable p1)
               (div-terms (:terms p1) (->constant-term p2)))
    :else
    (make-poly (:variable p2)
               (div-terms (->constant-term p1) (:terms p2)))))


(defn rem-terms
  [l1 l2]
  (sub-terms l1 (mul-terms l2 (div-terms l1 l2))))


(defn rem-poly
  [p1 p2]
  (cond
    (or (= (:variable p1) (:variable p2))
        (constant?-poly p2))
    (make-poly (:variable p1) (rem-terms (:terms p1) (:terms p2)))
    (constant?-poly p1)
    (make-poly (:variable p2) (rem-terms (:terms p1) (:terms p2)))
    (< (compare (:variable p1) (:variable p2)) 0)
    (make-poly (:variable p1)
               (rem-terms (:terms p1) (->constant-term p2)))
    :else
    (make-poly (:variable p2)
               (rem-terms (->constant-term p1) (:terms p2)))))


(defn init!
  []
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
  (put! ['first 'sparse] sparse/first)
  (put! ['strip-zeros 'dense] dense/strip-zeros)
  (put! ['strip-zeros 'sparse] sparse/strip-zeros))


(comment (init!))

;; (strip-zeros '[sparse ({:coeff [complex [cartesian [0 0]]] :order 5} {:coeff 10 :order 4})])

(ns sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.dense
  (:refer-clojure :exclude [zero? first])
  (:require
    [sicp-solutions-clojure.chapter-2.algebraic-system.base
     :as base]))

;; In descending order

(def tag #(vector 'dense %))


(defn strip-zeros
  [t]
  (loop [result t]
    (if (= (clojure.core/first result) 0)
      (recur (rest result))
      result)))


(defn vector->dense-terms
  [v]
  (assert (= (type v) clojure.lang.PersistentVector))
  (tag (strip-zeros v)))


(defn add-terms
  ([] [])
; Reduce on empty collection cause error

  ([t1] t1)
  ([t1 t2]
   (strip-zeros
     (let [len1 (count t1)
           len2 (count t2)]
       (cond
         (< len1 len2)
         (vec (concat (subvec t2 0 (- len2 len1))
                      (map base/add (subvec t2 (- len2 len1)) t1)))
         (> len1 len2)
         (vec (concat (subvec t1 0 (- len1 len2))
                      (map base/add (subvec t1 (- len1 len2)) t2)))
         :else
         (vec (map base/add t1 t2)))))))


(defn neg-terms
  [t]
  (vec (map base/neg t)))


(defn mul-terms
  [t1 t2]
  ;(prn t1 t2)
  (let [len2 (count t2)
        mul-term-terms
        (fn [idx itm]
          (vec (concat (map (partial base/mul itm) t1)
                       (repeat (- len2 idx 1) 0))))]
    (->> t2
         (map-indexed mul-term-terms)
         (reduce add-terms)
         strip-zeros)))


(defn constant
  [t]
  (if (empty? t) 0 (last t)))


(defn add-constant
  [t c]
  (conj (subvec t 0 (count t)) (base/add (last t) ['polynomial c])))


(defn constant?
  [t]
  (<= (count t) 1))


(defn zero?
  [t]
  (every? (partial = 0) t))


(defn first
  [t]
  {:order (- (count t) 1),
   :coeff (clojure.core/first t)})


(comment
  (let [a [1 2 3 4]
        b [5 0 3 2]]
    (prn (add-terms a b))))

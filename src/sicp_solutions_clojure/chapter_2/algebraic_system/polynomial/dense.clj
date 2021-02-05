(ns sicp-solutions-clojure.chapter-2.algebraic-system.polynomial.dense
  (:refer-clojure)
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system
             [base :as base])))

(def tag #(vector 'dense %))

(defn vector->sparse-terms [v]
  (assert (= (type v) clojure.lang.PersistentVector))
  (tag v))

(defn add-terms [t1 t2]
  (let [len1 (count t1)
        len2 (count t2)]
    (cond 
      (< len1 len2)
        (concat (subvec t2 0 (- len2 len1)) 
                (vec (map + (subvec t2 (- len2 len1)) t1)))
      (> len1 len2)
        (concat (subvec t1 0 (- len1 len2)) 
                (vec (map + (subvec t1 (- len1 len2)) t2)))
      :else
        (vec (map + t1 t2)))))


(defn mul-terms [t1 t2]
  (let [len2 (count t2)]
    (->> t2
         (map-indexed (fn [idx itm]
                        ))
         (reduce add-terms))))

(defn find-constant [t] (last t))

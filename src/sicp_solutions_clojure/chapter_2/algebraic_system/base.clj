(ns sicp-solutions-clojure.chapter-2.algebraic-system.base
  (:refer-clojure :exclude [type get val]))

(def ops-types-map {})

(defn get [symbols]
  (clojure.core/get ops-types-map symbols))

(defn put! [symbols val]
  (alter-var-root (var ops-types-map) (constantly (assoc ops-types-map symbols val))))

(defn type [x]
  (if (number? x) 'primitive (first x)))
(defn val [x]
  (if (number? x) x (second x)))

(defn apply-general [sym x y] 
  ((get [sym (type x) (type y)]) (val x) (val y)))

(defn add [x y] (apply-general 'add x y))
(defn sub [x y] (apply-general 'sub x y))
(defn mul [x y] (apply-general 'mul x y))
(defn div [x y] (apply-general 'div x y))
(defn make [key & args]
  (apply (get (concat ['make] key)) args))

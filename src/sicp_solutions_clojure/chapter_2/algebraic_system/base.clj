(ns sicp-solutions-clojure.chapter-2.algebraic-system.base
  (:refer-clojure :exclude [type get val vals zero? ]))

(def ops-types-map {})

(defn get [symbols]
  (clojure.core/get ops-types-map symbols))

(defn put! [symbols val]
  (alter-var-root (var ops-types-map) 
                  (constantly (assoc ops-types-map symbols val))))

(def coercion-map {})

(defn get-coercion [symbols]
  (clojure.core/get coercion-map symbols))

(defn put-coercion! [symbols val]
  (alter-var-root (var coercion-map)
                  (constantly (assoc coercion-map symbols val))))

(defn type [x]
  (if (number? x) 'primitive (first x)))
(defn val [x]
  (if (number? x) x (second x)))

(defn apply-general-coercioned [sym & vars]
  (let [ops-types-key (vec (conj (map type vars) sym))]
    (if-let [f (get ops-types-key)]
      (apply f (map val vars))
      (throw (Exception. (str 
                          "apply-general-coercioned: no function for key: " 
                          ops-types-key))))))

(defn apply-general [sym & vars] 
  (if (= 2 (count vars))
    (let [v1 (first vars)
          v2 (second vars)
          t1 (type v1) 
          t2 (type v2)]
      (if-let [f (get [sym t1 t2])]
        (f (val v1) (val v2))
        (if-let [g (get-coercion [t1 t2])]
          (apply-general-coercioned sym (g v1) v2)
          (if-let [h (get-coercion [t2 t1])]
            (apply-general-coercioned sym v1 (h v2))
            (throw (Exception. (str 
                                "apply-general: no function for key: " 
                                [sym t1 t2])))))))
    (apply-general-coercioned sym vars)))

(defn make [key & args]
  (apply (get (concat ['make] key)) (map val args)))

(defn add [x y] (apply-general 'add x y))
(defn sub [x y] (apply-general 'sub x y))
(defn mul [x y] (apply-general 'mul x y))
(defn div [x y] (apply-general 'div x y))
(defn eq? [x y] (apply-general 'eq? x y))
(defn zero? [x] (apply-general 'zero? x))

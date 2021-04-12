(ns sicp-solutions-clojure.chapter-2.algebraic-system.base
  (:refer-clojure :exclude [type get val vals zero? number? rem]))


(def ops-types-map {})


(defn get
  [symbols]
  (clojure.core/get ops-types-map symbols))


(defn put!
  [symbols val]
  (alter-var-root (var ops-types-map)
                  (constantly (assoc ops-types-map symbols val))))




(comment
  (def coercion-map {})

  (defn get-coercion [symbols]
    (clojure.core/get coercion-map symbols))

  (defn put-coercion! [symbols val]
    (alter-var-root (var coercion-map)
                    (constantly (assoc coercion-map symbols val)))))


(defn type
  [x]
  (if (clojure.core/number? x) 'primitive (first x)))


(defn val
  [x]
  (if (clojure.core/number? x) x (second x)))


(defn wrap
  [t v]
  (if (= 'primitive t) v (vector t v)))


(defn number?
  [x]
  (boolean (or (clojure.core/number? x) (get ['zero? (type x)]))))


(defn apply-general-converted
  [sym & vars]
  ; Duties: apply the sym to vars, and then wrap them up.
  (let [ops-types-key (vec (conj (map type vars) sym))]
    ;; (prn "apply-general-converted:" ops-types-key sym vars (get ops-types-key))
    (if-let [f (get ops-types-key)]
      ;;(do
      ;; (prn "applied-general-converted: " (apply f (map val vars)) )
      (let [result (apply f (map val vars))]
        ;; (prn 'base/apply-general-converted sym vars)
        ;; (prn "agc result: " result)
        (cond (boolean? result)
              result
              (= sym 'project)
              (if (#{'polynomial 'ratio-func} (type (first vars)))
                result
                (wrap (get ['project-to (type (first vars))]) result))
              :else
              (wrap (type (first vars)) result)))
      ;;)
      (throw (Exception. (str
                           "apply-general-converted: no function for key: "
                           ops-types-key))))))

;; apply-general: raising version
;; It's but a practice, so I'll try a hierarchy that is consitent with previous
;; work, it looks like this:
;; rational -> primitive -> complex
(defn level
  [x]
  (get ['level (type x)]))


(defn raise
  [x]
; Always remember to unwrap the variables
  ;; (prn "raise :" x)
  ((get ['raise (type x)]) (val x)))


(comment (let [lock (Object.)]
           (defn sync-println [& args]
             (locking lock (apply println args)))))


(declare lower)


(defn apply-general
  [sym & vars]
  ;Duties: raise variable types, Unwrap the tagged numbers
  ; (sync-println sym vars)
  ;; (prn "base/apply-general" sym vars)
  (if (= 2 (count vars))
    (let [v1 (first vars)
          v2 (second vars)]
      (cond (< (level v1) (level v2)) (recur sym (list (raise v1) v2))
            (= (level v1) (level v2)) (lower (apply-general-converted sym v1 v2))
            :else (recur sym (list v1 (raise v2)))))
    (lower (apply apply-general-converted sym vars))))


(defn make
  [key & args]
  (let [inner (apply (get (concat ['make] key)) args)
        t (first key)]
    (if (= t 'primitive) inner (vector t inner))))


(defn add
  [x y]
  (apply-general 'add x y))


(defn neg
  [x]
  (apply-general 'neg x))


(defn sub
  [x y]
  (add x (neg y)))


(defn mul
  [x y]
  ;; (prn 'mul x y)
  (apply-general 'mul x y))


(defn div
  [x y]
  ;; (prn 'div x y)
  (apply-general 'div x y))


(defn remainder
  [x y]
  (apply-general 'rem x y))


(defn eq?
  [x y]
  ;; (prn 'eq? x y)
  (apply-general 'eq? x y))


(defn alg-zero?
  [x]
  (apply-general 'zero? x))


(defn project
  [x]
  (apply-general 'project x))


(defn lower
  [x]
  ;; (prn "lowering " x)
  (if (or (boolean? x) (= (level x) 1) (= (level x) 5))
    ;; I've closed the lower function on ratio-function since it's too slow,
    ;; and actually can cause some problems.
    x
    (let [p (project x)]
      (if (eq? p x) (recur p) x))));; As they say, we know the cost of nothing.
;; (use 'clojure.tools.trace)
;; (trace-ns sicp-solutions-clojure.chapter-2.algebraic-system.base)


(defn ->primitive [x]
  (prn x (type x))
  (cond (= (type x) 'primitive) x
        (= (type x) 'rational) (raise x)
        :else (throw (Exception. "Can't convert to primitive"))))

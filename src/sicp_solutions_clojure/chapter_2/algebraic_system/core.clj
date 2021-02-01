(ns sicp-solutions-clojure.chapter-2.algebraic-system.core 
  (:refer-clojure :exclude [test])
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system 
             [base :as base :refer [add sub mul div make]]
             [primitive :as primitive]
             [ratio :as ratio])
            (sicp-solutions-clojure.chapter-2.algebraic-system.complex 
             [core :as complex])))

(defn init! []
  ; Install primitive module 
  (base/put! ['make 'primitive] identity)
  (base/put! ['add 'primitive 'primitive] +)
  (base/put! ['sub 'primitive 'primitive] -)
  (base/put! ['mul 'primitive 'primitive] *)
  (base/put! ['div 'primitive 'primitive] /)
  ; Install ratio module 
  (base/put! ['make 'rational] #(ratio/tag (ratio/make %1 %2)))
  (base/put! ['add 'rational 'rational] #(ratio/tag (ratio/add %1 %2)))
  (base/put! ['sub 'rational 'rational] #(ratio/tag (ratio/sub %1 %2)))
  (base/put! ['mul 'rational 'rational] #(ratio/tag (ratio/mul %1 %2)))
  (base/put! ['div 'rational 'rational] #(ratio/tag (ratio/div %1 %2)))
  ; Install complex module 
  (complex/init!)
  (base/put! ['make 'complex 'real-imag] #(complex/tag (complex/real-imag->complex %1 %2)))
  (base/put! ['make 'complex 'mag-ang] #(complex/tag (complex/mag-ang->complex %1 %2)))
  (base/put! ['add 'complex 'complex] #(complex/tag (complex/add %1 %2)))
  (base/put! ['sub 'complex 'complex] #(complex/tag (complex/sub %1 %2)))
  (base/put! ['mul 'complex 'complex] #(complex/tag (complex/mul %1 %2)))
  (base/put! ['div 'complex 'complex] #(complex/tag (complex/div %1 %2))))

(defn test []
  (init!)
  (println (add (make ['primitive] 123) (make ['primitive] 456)))
  (println (mul (make ['rational] 3 4) (make ['rational] 12 24)))
  (println (div (make ['complex 'real-imag] 1 0) (make ['complex 'mag-ang] 5 (/ (Math/PI) 3)))))

(comment (test))

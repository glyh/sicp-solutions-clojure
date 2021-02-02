(ns sicp-solutions-clojure.chapter-2.algebraic-system.coercion
  (:refer-clojure)
  (:require (sicp-solutions-clojure.chapter-2.algebraic-system.complex
             [core :as complex])))

(defn primitive->complex [x]
  (complex/tag (complex/real-imag->complex x 0)))



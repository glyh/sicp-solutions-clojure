(ns sicp-solutions-clojure.core
  (:require (sicp-solutions-clojure.chapter-2
              [picture-language :as pic-lang])
            [sicp-solutions-clojure.chapter-2.algebraic-system.core :as alg-sys])
  (:gen-class))

(def menu {:pic-lang pic-lang/show-sketch
           alg-sys/alg-test :alg-sys})

(defn print-help []
  1)

(defn -main [& args]
  (if (seq args)
    (let [subprogram (keyword (first args))]
      ((get menu subprogram)))
    (print-help)))

(comment (-main "pic-lang"))
(comment (-main "alg-sys"))

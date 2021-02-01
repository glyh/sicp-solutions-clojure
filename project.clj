(defproject sicp-solutions-clojure "1.0.0"
  :description "Tiny SICP solutions (project based) written in Clojure."
  :url "https://github.com/glyh/sicp-solutions-clojure"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [quil "3.1.0"]]
  :main ^:skip-aot sicp-solutions-clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :plugins [[cider/cider-nrepl "0.24.0"]])

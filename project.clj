(defproject ssa-baby-names-analysis "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-http "1.1.0"]
                 [hickory "0.5.4"]]
  :main ^:skip-aot ssa-baby-names-analysis.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

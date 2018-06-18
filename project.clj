(defproject clll "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]]
  :main ^:skip-aot clll.core
  :target-path "target/%s"
  :plugins [[lein-cljfmt "0.5.7"]]
  :profiles {:uberjar {:aot :all}})

(defproject hnefatafl "0.1.0"
  :description "A Fetlar Hnefatafl Game in Clojure"
  :url "github.com/wegi"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [midje "1.6.2"]
                 [org.clojars.pjlegato/clansi "1.3.0"]
                 [quil "2.2.1"]]
  :main ^:skip-aot hnefatafl.ui
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies  [[midje "1.6.2"]]
                   :plugins       [[lein-midje "3.1.3"]]}})

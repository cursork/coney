(defproject coney "1.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/lshift/coney"
  :license {:name "Mozilla Public License Version 1.1"
            :url "https://www.mozilla.org/MPL/1.1/"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/data.codec "0.1.0"]
                 [digest "1.4.4"]
                 [http-kit "2.1.16"]
                 [cheshire "5.4.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 ]
  :plugins [
            [lein-bin "0.3.5"]
            [lein-release "1.0.5"]
          ]
  :lein-release {:deploy-via :shell :shell ["lein" "bin"]}
  :bin { :name "coney"
         :bin-path "target" }
  :profiles {
    :test {
      :dependencies [
        [midje "1.7.0-beta1" :exclusions [org.clojure/clojure]]
        [http-kit.fake "0.2.1"]
      ]
      :plugins [
        [lein-midje "3.1.3"]
        [lein-cloverage "1.0.6"]
      ]
    }
    :dev [:test
      {
        :dependencies [
          [lein-light-nrepl "0.1.0"]
        ]
        :repl-options {:nrepl-middleware [lighttable.nrepl.handler/lighttable-ops]}
      }
    ]
    :uberjar {:aot :all}
  }
  :main ^:skip-aot coney.core
  :target-path "target/%s"
)

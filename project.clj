(defproject cljam "0.1.4-SNAPSHOT"
  :description "A DNA Sequence Alignment/Map (SAM) library for Clojure"
  :url "https://github.com/chrovis/cljam"
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/tools.cli "0.3.3"]
                 [me.raynes/fs "1.4.6"]
                 [pandect "0.5.3"]
                 [clj-sub-command "0.2.1"]
                 [bgzf4j "0.1.0"]
                 [com.climate/claypoole "1.0.0"]]
  :plugins [[lein-midje "3.1.3"]
            [lein-bin "0.3.5"]
            [lein-marginalia "0.8.0"]]
  :profiles {:dev {:dependencies [[midje "1.7.0" :exclusions [slingshot]]
                                  [criterium "0.4.3"]
                                  [cavia "0.1.5"]
                                  [primitive-math "0.1.4"]]
                   :global-vars {*warn-on-reflection* true}}}
  :main cljam.main
  :aot [cljam.main]
  :bin {:name "cljam"}
  :repl-options {:init-ns user}
  :signing {:gpg-key "developer@xcoo.jp"})

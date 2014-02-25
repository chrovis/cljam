(defproject cljam "0.1.0-SNAPSHOT"
  :description "A DNA Sequence Alignment/Map (SAM) library for Clojure"
  :url "https://chrov.is"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/tools.cli "0.3.0"]
                 [clj-sub-command "0.2.0"]
                 [bgzf4j "0.1.0"]]
  :plugins [[lein-midje "3.1.3"]
            [lein-bin "0.3.4"]]
  :profiles {:dev {:dependencies [[midje "1.6.2"]
                                  [criterium "0.4.2"]
                                  [cavia "0.1.1"]]
                   :global-vars {*warn-on-reflection* true}}}
  :main cljam.core
  :aot [cljam.core]
  :bin {:name "cljam"}
  :repl-options {:init-ns user})

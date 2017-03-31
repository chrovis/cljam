(defproject cljam "0.2.1-SNAPSHOT"
  :description "A DNA Sequence Alignment/Map (SAM) library for Clojure"
  :url "https://github.com/chrovis/cljam"
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"}
  :dependencies [[org.clojure/tools.logging "0.3.1"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.apache.commons/commons-compress "1.13"]
                 [me.raynes/fs "1.4.6"]
                 [clj-sub-command "0.3.0"]
                 [digest "1.4.5"]
                 [bgzf4j "0.1.0"]
                 [com.climate/claypoole "1.1.4"]
                 [camel-snake-kebab "0.4.0"]]
  :plugins [[lein-midje "3.2.1"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [midje "1.8.3" :exclusions [slingshot]]
                                  [cavia "0.3.1"]]
                   :plugins [[lein-bin "0.3.5"]
                             [lein-codox "0.10.3"]
                             [lein-marginalia "0.9.0" :exclusions [org.clojure/clojure]]
                             [lein-cloverage "1.0.9" :exclusions [org.clojure/clojure]]]
                   :main ^:skip-aot cljam.main
                   :global-vars {*warn-on-reflection* true}}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                                  [midje "1.9.0-alpha6"]]}
             :uberjar {:main cljam.main
                       :aot :all}}
  :aliases {"docs" ["do" "codox" ["marg" "-d" "target/literate" "-m"]]}
  :bin {:name "cljam"}
  :codox {:namespaces [#"^cljam\.(?!cli)(?!lsb)(?!main)(?!util)[^\.]+$"]
          :output-path "target/docs"
          :source-uri "https://github.com/chrovis/cljam/blob/{version}/{filepath}#L{line}"}
  :repl-options {:init-ns user}
  :signing {:gpg-key "developer@xcoo.jp"})

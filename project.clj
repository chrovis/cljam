(defproject cljam "0.7.1-SNAPSHOT"
  :description "A DNA Sequence Alignment/Map (SAM) library for Clojure"
  :url "https://github.com/chrovis/cljam"
  :license {:name "Apache License, Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/tools.logging "0.4.1"]
                 [org.clojure/tools.cli "0.4.2"]
                 [org.apache.commons/commons-compress "1.18"]
                 [clj-sub-command "0.5.1"]
                 [digest "1.4.8"]
                 [bgzf4j "0.1.0"]
                 [com.climate/claypoole "1.1.4"]
                 [camel-snake-kebab "0.4.0"]
                 [proton "0.1.8"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [cavia "0.5.1"]
                                  [criterium "0.4.4"]
                                  [net.totakke/libra "0.1.1"]
                                  [se.haleby/stub-http "0.2.5"]]
                   :plugins [[lein-binplus "0.6.4" :exclusions [org.clojure/clojure]]
                             [lein-codox "0.10.5"]
                             [lein-marginalia "0.9.1" :exclusions [org.clojure/clojure]]
                             [lein-cloverage "1.0.13"]
                             [net.totakke/lein-libra "0.1.2"]]
                   :test-selectors {:default #(not-any? % [:slow :remote])
                                    :slow :slow ; Slow tests with local resources
                                    :remote :remote ; Tests with remote resources
                                    :all (constantly true)}
                   :main ^:skip-aot cljam.tools.main
                   :global-vars {*warn-on-reflection* true}}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.1-beta1"]]}
             :uberjar {:dependencies [[org.clojure/clojure "1.9.0"]
                                      [org.apache.logging.log4j/log4j-api "2.11.1"]
                                      [org.apache.logging.log4j/log4j-core "2.11.1"]]
                       :resource-paths ["bin-resources"]
                       :main cljam.tools.main
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]
                       :aot :all}}
  :deploy-repositories [["snapshots" {:url "https://clojars.org/repo/"
                                      :username [:env/clojars_username :gpg]
                                      :password [:env/clojars_password :gpg]}]]
  :aliases {"docs" ["do" "codox" ["marg" "-d" "target/literate" "-m"]]}
  :bin {:name "cljam"
        :bootclasspath true}
  :codox {:namespaces [#"^cljam\.(?!tools)[\w\-]+(\.[\w\-]+)?$"]
          :output-path "target/docs"
          :source-uri "https://github.com/chrovis/cljam/blob/{version}/{filepath}#L{line}"}
  :repl-options {:init-ns user}
  :signing {:gpg-key "developer@xcoo.jp"})

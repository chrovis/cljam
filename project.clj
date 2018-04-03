(defproject cljam "0.5.2-SNAPSHOT"
  :description "A DNA Sequence Alignment/Map (SAM) library for Clojure"
  :url "https://github.com/chrovis/cljam"
  :license {:name "Apache License, Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/tools.logging "0.4.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.apache.commons/commons-compress "1.16.1"]
                 [clj-sub-command "0.4.0"]
                 [digest "1.4.8"]
                 [bgzf4j "0.1.0"]
                 [com.climate/claypoole "1.1.4"]
                 [camel-snake-kebab "0.4.0"]
                 [proton "0.1.6"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [cavia "0.4.2"]
                                  [criterium "0.4.4"]
                                  [net.totakke/libra "0.1.0"]
                                  [org.tcrawley/dynapath "1.0.0"]]
                   :plugins [[lein-binplus "0.6.2" :exclusions [org.clojure/clojure]]
                             [lein-codox "0.10.3"]
                             [lein-marginalia "0.9.1" :exclusions [org.clojure/clojure]]
                             [lein-cloverage "1.0.10" :exclusions [org.clojure/clojure org.tcrawley/dynapath]]
                             [net.totakke/lein-libra "0.1.0"]]
                   :test-selectors {:default #(not-any? % [:slow :remote])
                                    :slow :slow ; Slow tests with local resources
                                    :remote :remote ; Tests with remote resources
                                    :all (constantly true)}
                   :main ^:skip-aot cljam.tools.main
                   :global-vars {*warn-on-reflection* true}}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0-RC1"]]}
             :uberjar {:dependencies [[org.clojure/clojure "1.8.0"]
                                      [org.apache.logging.log4j/log4j-api "2.11.0"]
                                      [org.apache.logging.log4j/log4j-core "2.11.0"]]
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

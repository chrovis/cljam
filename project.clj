(defproject cljam "0.8.5-SNAPSHOT"
  :description "A DNA Sequence Alignment/Map (SAM) library for Clojure"
  :url "https://github.com/chrovis/cljam"
  :license {:name "Apache License, Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/core.memoize "1.0.257"]
                 [org.clojure/tools.logging "1.2.4"]
                 [org.clojure/tools.cli "1.0.214"]
                 [org.apache.commons/commons-compress "1.23.0"]
                 [clj-sub-command "0.6.0"]
                 [digest "1.4.10"]
                 [bgzf4j "0.1.2"]
                 [com.climate/claypoole "1.1.4"]
                 [camel-snake-kebab "0.4.3"]
                 [proton "0.2.2"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.11.1"]
                                  [cavia "0.5.1"]
                                  [criterium "0.4.6"]
                                  [net.totakke/libra "0.1.1"]
                                  [se.haleby/stub-http "0.2.14"]]
                   :plugins [[lein-binplus "0.6.6" :exclusions [org.clojure/clojure]]
                             [lein-codox "0.10.7"]
                             [lein-marginalia "0.9.1" :exclusions [org.clojure/clojure]]
                             [lein-cloverage "1.1.2"]
                             [net.totakke/lein-libra "0.1.2"]
                             [lein-cljfmt "0.6.8"]]
                   :test-selectors {:default #(not-any? % [:slow :remote])
                                    :slow :slow ; Slow tests with local resources
                                    :remote :remote ; Tests with remote resources
                                    :all (constantly true)}
                   :main ^:skip-aot cljam.tools.main
                   :global-vars {*warn-on-reflection* true
                                 *unchecked-math* :warn-on-boxed}}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.3"]]}
             :uberjar {:dependencies [[org.clojure/clojure "1.11.1"]
                                      [org.apache.logging.log4j/log4j-api "2.20.0"]
                                      [org.apache.logging.log4j/log4j-core "2.20.0"]]
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

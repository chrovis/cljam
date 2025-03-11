(defproject cljam "0.9.0-SNAPSHOT"
  :description "A DNA Sequence Alignment/Map (SAM) library for Clojure"
  :url "https://github.com/chrovis/cljam"
  :license {:name "Apache License, Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/core.memoize "1.1.266"]
                 [org.clojure/tools.logging "1.3.0"]
                 [org.apache.commons/commons-compress "1.27.1"]
                 [org.tukaani/xz "1.10"] ; necessary for CRAM LZMA compression
                 [digest "1.4.10"]
                 [bgzf4j "0.1.2"]
                 [com.climate/claypoole "1.1.4"]
                 [camel-snake-kebab "0.4.3"]
                 [proton "0.2.3"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.12.0"]
                                  [cavia "0.7.2"]
                                  [criterium "0.4.6"]
                                  [net.totakke/libra "0.1.1"]
                                  [se.haleby/stub-http "0.2.14"]]
                   :plugins [[lein-codox "0.10.8"]
                             [lein-marginalia "0.9.2" :exclusions [org.clojure/clojure]]
                             [lein-cloverage "1.2.4"]
                             [net.totakke/lein-libra "0.1.2"]
                             [lein-cljfmt "0.9.2"]]
                   :test-selectors {:default #(not-any? % [:slow :remote])
                                    :slow :slow ; Slow tests with local resources
                                    :remote :remote ; Tests with remote resources
                                    :all (constantly true)}
                   :global-vars {*warn-on-reflection* true
                                 *unchecked-math* :warn-on-boxed}}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.3"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.4"]]}}
  :deploy-repositories [["snapshots" {:url "https://clojars.org/repo/"
                                      :username [:env/clojars_username :gpg]
                                      :password [:env/clojars_password :gpg]}]]
  :aliases {"docs" ["do" "codox" ["marg" "-d" "target/literate" "-m"]]}
  :codox {:output-path "target/docs"
          :source-uri "https://github.com/chrovis/cljam/blob/{version}/{filepath}#L{line}"}
  :repl-options {:init-ns user}
  :signing {:gpg-key "developer@xcoo.jp"})

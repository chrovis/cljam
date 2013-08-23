(defproject cljam "0.1.0-SNAPSHOT"
  :description "A DNA Sequence Alignment/Map (SAM) library for Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [clj-sub-command "0.1.0"]]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :javac-options ["-target" "1.7" "-source" "1.7"]
  :plugins [[lein-midje "3.0.1"]
            [lein-bin "0.3.4"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]
                                  [criterium "0.4.1"]]}}
  :main cljam.core
  :aot [cljam.core]
  :bin {:name "cljam"})

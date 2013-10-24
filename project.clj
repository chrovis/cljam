(defproject cljam "0.1.0-SNAPSHOT"
  :description "A DNA Sequence Alignment/Map (SAM) library for Clojure"
  :url "https://chrov.is"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["snapshots" {:url "https://nexus.xcoo.jp/content/repositories/snapshots"}]
                 ["releases" {:url "https://nexus.xcoo.jp/content/repositories/releases"}]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [clj-sub-command "0.1.0"]
                 [chrovis/bgzf4j "0.1.0-SNAPSHOT"]]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :javac-options ["-target" "1.7" "-source" "1.7"]
  :plugins [[lein-midje "3.0.1"]
            [lein-bin "0.3.4"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]
                                  [criterium "0.4.1"]]}}
  ;; :global-vars {*warn-on-reflection* true}
  :main cljam.core
  :aot [cljam.core]
  :bin {:name "cljam"})

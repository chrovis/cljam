(ns cljam.main
  "The entry point of the command-line tool."
  (:require [cljam.cli :as cli])
  (:gen-class))

(defn -main [& args]
  (cli/run args)
  (shutdown-agents))

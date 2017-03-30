(ns cljam.main
  "The entry point of the command-line tool."
  (:require [cljam.cli :as cli])
  (:gen-class))

(defn -main
  "The entry point of the command-line tool. This function calls
  `shutdown-agents` finally because some features use agents for concurrency."
  [& args]
  (cli/run args)
  (shutdown-agents))

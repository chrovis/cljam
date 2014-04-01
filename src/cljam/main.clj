(ns cljam.main
  (:require [cljam.cli :as cli])
  (:gen-class))

(defn -main [& args]
  (cli/run args)
  (shutdown-agents))

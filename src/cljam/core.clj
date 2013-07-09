(ns cljam.core
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:require [cljam.bam :as bam]))

(defn -view [args]
  (with-command-line args
    "Usage: todo"
    [[foo "foo" 1]]
    (println "view " foo)))

(defn -sort [args]
  (with-command-line args
    "Usage: todo"
    [[foo "foo" 1]]
    (println "sort " foo)))

(defn -index [args]
  (let [[in-bam _] args]
    "Usage: cljam index <in.bam>"
    (bam/index in-bam)))

(defn -pileup [args]
  (with-command-line args
    "Usage: todo"
    [[foo "foo" 1]]
    (println "index " foo)))

(defn -main [& args]
  (let [[subcmd & args] args]
    (condp = subcmd
      "view" (-view args)
      "sort" (-sort args)
      "index" (-index args)
      "idxstats" (-idxstats args)
      "merge" (-merge args)
      "faidx" (-merge args)
      "pileup" (-pileup args)
      "mpileup" (-mpileup args)
      "tview" (-tview args)
      (println subcmd "is invalid subcommand")))
  nil)

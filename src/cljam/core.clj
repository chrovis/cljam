(ns cljam.core
  (:use [clojure.contrib.command-line :only (with-command-line)])
  (:require [cljam.io :as io]
            [cljam.sam :as sam]
            [cljam.bam :as bam]))

(defn -view [args]
  (with-command-line args
    "Usage: view [--header] [--format <auto|sam|bam>] sam|bam"
    [[header? "Include header in the output" false]
     [format "Specify input file format from <auto|sam|bam>" "auto"]
     file]
    (let [asam (condp = format
                   "auto" (io/slurp     (first file))
                   "sam"  (io/slurp-sam (first file))
                   "bam"  (io/slurp-bam (first file)))]
      (when header?
        (doseq [sh (:header asam)]
          (println (sam/stringify sh))))
      (doseq [sa (:alignments asam)]
        (println (sam/stringify sa))))))

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

(defn -idxstats [args] "Usage :todo")
(defn -merge [args] "Usage :todo")
(defn -mpileup [args] "Usage :todo")
(defn -tview [args] "Usage :todo")

(defn -main [& args]
  (let [[subcmd & args] args]
    (condp = subcmd
      "view"     (-view     args)
      "sort"     (-sort     args)
      "index"    (-index    args)
      "idxstats" (-idxstats args)
      "merge"    (-merge    args)
      "faidx"    (-merge    args)
      "pileup"   (-pileup   args)
      "mpileup"  (-mpileup  args)
      "tview"    (-tview    args)
      (println subcmd "is invalid subcommand")))
  nil)

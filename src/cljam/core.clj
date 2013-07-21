(ns cljam.core
  (:refer-clojure :exclude [sort merge])
  (:use [clojure.contrib.command-line :only (with-command-line)]
        clj-sub-command.core)
  (:require [cljam.io :as io]
            [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.sorter :as sorter]))

(defn view [& args]
  (with-command-line args
    "Usage: view [--header] [--format <auto|sam|bam>] <in.bam|sam>"
    [[header? "Include header in the output" false]
     [format "Specify input file format from <auto|sam|bam>" "auto"]
     files]
    (let [asam (condp = format
                   "auto" (io/slurp     (first files))
                   "sam"  (io/slurp-sam (first files))
                   "bam"  (io/slurp-bam (first files)))]
      (when header?
        (doseq [sh (:header asam)]
          (println (sam/stringify sh))))
      (doseq [sa (:alignments asam)]
        (println (sam/stringify sa))))))

(defn convert [& args]
  (with-command-line args
    "Usage: convert [--src-format <auto|sam|bam>] [--dst-format <auto|sam|bam>] <in.bam|sam> <out.bam|sam>"
    [[src-format "Specify input file format from <auto|sam|bam>" "auto"]
     [dst-format "Specify output file format from <auto|sam|bam>" "auto"]
     files]
    (let [asam (io/slurp (first files))]
      (io/spit (second files) asam))))

(defn sort [& args]
  (with-command-line args
    "Usage: sort [--order <coordinate|queryname>] <in.bam|sam> <out.bam|sam>"
    [[order "Specify sorting order of alignments from <coordinate|queryname>" "coordinate"]
     files]
    (let [asam (io/slurp (first files))]
      (condp = order
        "coordinate" (io/spit (second files) (sorter/sort-by-pos asam))
        "queryname"  (io/spit (second files) (sorter/sort-by-qname asam))))))

(defn index [& args]
  (let [[in-bam _] args]
    "Usage: cljam index <in.bam>"
    nil))

(defn pileup [& args]
  (with-command-line args
    "Usage: todo"
    [[foo "foo" 1]]
    (println "index " foo)))

(defn idxstats [& args] "Usage :todo")
(defn merge [& args] "Usage :todo")

(defn -main [& args]
  (do-sub-command args
    "Usage: cljam [-h] {view,convert,sort,index,idxstats,merge,pileup} ..."
    [:view "Extract/print all or sub alignments in SAM or BAM format." view]
    [:convert "Convert SAM to BAM or BAM to SAM." convert]
    [:sort "Sort alignments by leftmost coordinates." sort]
    [:index "" index]
    [:idxstats "" idxstats]
    [:merge "" merge]
    [:pileup "" pileup]))

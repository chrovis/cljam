(ns cljam.core
  (:refer-clojure :exclude [sort merge])
  (:require [clojure.contrib.command-line :refer [with-command-line]]
            [clj-sub-command.core :refer [do-sub-command]]
            (cljam [io :as io]
                   [sam :as sam]
                   [bam :as bam]
                   [sorter :as sorter]
                   [indexer :as indexer]
                   [pileup :as pileup]))
  (:import (net.sf.picard.sam BuildBamIndex BamIndexStats)))

(defn view [& args]
  (with-command-line args
    "Usage: cljam view [--header] [--format <auto|sam|bam>] <in.bam|sam>"
    [[header? "Include header in the output" false]
     [format "Specify input file format from <auto|sam|bam>" "auto"]
     files]
    (when-not (= (count files) 1)
      (println "Invalid arguments")
      (System/exit 1))
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
    "Usage: cljam convert [--src-format <auto|sam|bam>] [--dst-format <auto|sam|bam>] <in.bam|sam> <out.bam|sam>"
    [[src-format "Specify input file format from <auto|sam|bam>" "auto"]
     [dst-format "Specify output file format from <auto|sam|bam>" "auto"]
     files]
    (when-not (= (count files) 2)
      (println "Invalid arguments")
      (System/exit 1))
    (let [asam (io/slurp (first files))]
      (io/spit (second files) asam))))

(defn sort [& args]
  (with-command-line args
    "Usage: cljam sort [--order <coordinate|queryname>] <in.bam|sam> <out.bam|sam>"
    [[order "Specify sorting order of alignments from <coordinate|queryname>" "coordinate"]
     files]
    (when-not (= (count files) 2)
      (println "Invalid arguments")
      (System/exit 1))
    (let [asam (io/slurp (first files))]
      (condp = order
        "coordinate" (io/spit (second files) (sorter/sort-by-pos asam))
        "queryname"  (io/spit (second files) (sorter/sort-by-qname asam))))))

(defn index [& args]
  (with-command-line args
    "Usage: cljam index <in.bam>"
    [files]
    (when-not (= (count files) 1)
      (println "Invalid arguments")
      (System/exit 1))
    ;; HACK: Should not use Picard
    (.. (BuildBamIndex.)
        (instanceMain (into-array String [(str "I=" (first files)),
                                          (str "O=" (first files) ".bai")])))))

(defn idxstats [& args]
  (with-command-line args
    "Usage: cljam idxstats <aln.bam>"
    [files]
    (when-not (= (count files) 1)
      (println "Invalid arguments")
      (System/exit 1))
    ;; HACK: Should not use Picard
    (.. (BamIndexStats.)
        (instanceMain (into-array String [(str "I=" (first files))])))))

(defn merge [& args]
  (with-command-line args
    "Usage: cljam merge <in1.bam|sam> <in2.bam|sam> ... <out.bam|sam>"
    [files]
    (when (< (count files) 2)
      (println "Invalid arguments")
      (System/exit 1))))

(defn pileup [& args]
  (with-command-line args
    "Usage: cljam pileup <in.bam>"
    [files]
    (when-not (= (count files) 1)
      (println "Invalid arguments")
      (System/exit 1))
    (let [sam (io/slurp-bam (first files))]
      (doseq [p (pileup/pileup (if-not (sorter/sorted? sam)
                                 (sorter/sort sam)
                                 sam))]
        (println p)))))

(defn faidx [& args]
  (with-command-line args
    "Usage: cljam faidx <ref.fasta>"
    [files]
    (when-not (= (count files) 1)
      (println "Invalid arguments")
      (System/exit 1))
    (io/spit-fai (str (first files) ".fai")
                 (io/slurp-fasta (first files)))))

(defn -main [& args]
  (do-sub-command args
    "Usage: cljam [-h] {view,convert,sort,index,idxstats,merge,pileup} ..."
    [:view     cljam.core/view     "Extract/print all or sub alignments in SAM or BAM format."]
    [:convert  cljam.core/convert  "Convert SAM to BAM or BAM to SAM."]
    [:sort     cljam.core/sort     "Sort alignments by leftmost coordinates."]
    [:index    cljam.core/index    "Index sorted alignment for fast random access."]
    [:idxstats cljam.core/idxstats "Retrieve  and print stats in the index file."]
    [:merge    cljam.core/merge    "Merge multiple SAM/BAM."]
    [:pileup   cljam.core/pileup   "todo"]
    [:faidx    cljam.core/faidx    "Index reference sequence in the FASTA format."]))

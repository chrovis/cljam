(ns cljam.core
  (:refer-clojure :exclude [sort merge slurp spit])
  (:require [clojure.contrib.command-line :refer [with-command-line]]
            [clj-sub-command.core :refer [do-sub-command]]
            (cljam [sam :as sam]
                   [bam :as bam]
                   [sorter :as sorter]
                   [indexer :as idxr]
                   [fasta :as fa]
                   [fasta-indexer :as fai]
                   [pileup :as pileup])))

(defn- slurp
  [f]
  (condp re-find f
    #"\.sam$" (sam/slurp f)
    #"\.bam$" (bam/slurp f)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn- spit
  [f sam]
  (condp re-find f
    #"\.sam$" (sam/spit f sam)
    #"\.bam$" (bam/spit f sam)
    (throw (IllegalArgumentException. "Invalid file type"))))

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
                   "auto" (slurp     (first files))
                   "sam"  (sam/slurp (first files))
                   "bam"  (bam/slurp (first files)))]
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
    (let [asam (slurp (first files))]
      (spit (second files) asam))))

(defn sort [& args]
  (with-command-line args
    "Usage: cljam sort [--order <coordinate|queryname>] <in.bam|sam> <out.bam|sam>"
    [[order "Specify sorting order of alignments from <coordinate|queryname>" "coordinate"]
     files]
    (when-not (= (count files) 2)
      (println "Invalid arguments")
      (System/exit 1))
    (let [asam (slurp (first files))]
      (condp = order
        "coordinate" (spit (second files) (sorter/sort-by-pos asam))
        "queryname"  (spit (second files) (sorter/sort-by-qname asam))))))

(defn index [& args]
  (with-command-line args
    "Usage: cljam index <in.bam>"
    [files]
    (when-not (= (count files) 1)
      (println "Invalid arguments")
      (System/exit 1))
    (idxr/build-bam-index (first files) (str (first files) ".bai"))))

(defn idxstats [& args]
  (with-command-line args
    "Usage: cljam idxstats <aln.bam>"
    [files]
    (when-not (= (count files) 1)
      (println "Invalid arguments")
      (System/exit 1))
    (idxr/bam-index-stats (first files))))

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
    (let [sam (bam/slurp (first files))]
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
    (fai/spit (str (first files) ".fai")
              (fa/slurp (first files)))))

(defn -main [& args]
  (do-sub-command args
    "Usage: cljam [-h] {view,convert,sort,index,idxstats,merge,pileup,faidx} ..."
    [:view     cljam.core/view     "Extract/print all or sub alignments in SAM or BAM format."]
    [:convert  cljam.core/convert  "Convert SAM to BAM or BAM to SAM."]
    [:sort     cljam.core/sort     "Sort alignments by leftmost coordinates."]
    [:index    cljam.core/index    "Index sorted alignment for fast random access."]
    [:idxstats cljam.core/idxstats "Retrieve  and print stats in the index file."]
    [:merge    cljam.core/merge    "Merge multiple SAM/BAM."]
    [:pileup   cljam.core/pileup   "Generate pileup for the BAM file."]
    [:faidx    cljam.core/faidx    "Index reference sequence in the FASTA format."]))

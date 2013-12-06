(ns cljam.core
  (:refer-clojure :exclude [sort merge slurp spit])
  (:require [clj-sub-command.core :refer [sub-command]]
            [clojure.tools.cli :refer [cli]]
            (cljam [sam :as sam]
                   [io :as io]
                   [bam :as bam]
                   [bam-indexer :as bai]
                   [sorter :as sorter]
                   [fasta :as fa]
                   [fasta-indexer :as fai]
                   [dict :as dict]
                   [pileup :as plp])
            [cljam.util.sam-util :refer [stringify-header stringify-alignment]])
  (:gen-class))

(defmacro ^:private exit-with
  [status & body]
  `(do (eval (conj (vec '~body) '(System/exit ~status)))
       nil))

(defn reader [f]
  (condp re-find f
    #"\.sam$" (sam/reader f)
    #"\.bam$" (bam/reader f)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn writer [f]
  (condp re-find f
    #"\.sam$" (sam/writer f)
    #"\.bam$" (bam/writer f)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defmulti read-header type)

(defmethod read-header cljam.sam.reader.SAMReader
  [rdr]
  (io/read-header rdr))

(defmethod read-header cljam.bam.reader.BAMReader
  [rdr]
  (io/read-header rdr))

(defmulti read-alignments type)

(defmethod read-alignments cljam.sam.reader.SAMReader
  [rdr]
  (io/read-alignments rdr {}))

(defmethod read-alignments cljam.bam.reader.BAMReader
  [rdr]
  (io/read-alignments rdr {}))

(defmulti read-refs type)

(defmethod read-refs cljam.sam.reader.SAMReader
  [rdr]
  (io/read-refs rdr))

(defmethod read-refs cljam.bam.reader.BAMReader
  [rdr]
  (io/read-refs rdr))

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

(defn view [args]
  (let [[opt [f _] help] (cli args
                              "Usage: cljam view [--header] [--format <auto|sam|bam>] <in.bam|sam>"
                              ["-h" "--help" "Print help" :default false :flag true]
                              ["--header" "Include header" :default false :flag true]
                              ["-f" "--format" "Input file format <auto|sam|bam>" :default "auto"])]
    (when (:help opt)
      (exit-with 0 (println help)))
    (with-open [r (condp = (:format opt)
                    "auto" (reader     f)
                    "sam"  (sam/reader f)
                    "bam"  (bam/reader f))]
      (when (:header opt)
        (println (stringify-header (read-header r))))
      (doseq [aln (read-alignments r)]
        (println (stringify-alignment aln))))
    nil))

(defn convert [args]
  (let [[opt [in out _] help] (cli args
                                    "Usage: cljam convert [--input-format <auto|sam|bam>] [--output-format <auto|sam|bam>] <in.bam|sam> <out.bam|sam>"
                                    ["-h" "--help" "Print help" :default false :flag true]
                                    ["-if" "--input-format" "Input file format <auto|sam|bam>" :default "auto"]
                                    ["-of" "--output-format" "Output file format <auto|sam|bam>" :default "auto"])]
    (when (:help opt)
      (exit-with 0 (println help)))
    (let [asam (slurp in)]
      (spit out asam))
    nil))

(defn sort [args]
  (let [[opt [in out _] help] (cli args
                                   "Usage: cljam sort [--order <coordinate|queryname>] <in.bam|sam> <out.bam|sam>"
                                   ["-h" "--help" "Print help" :default false :flag true]
                                   ["-o" "--order" "Sorting order of alignments <coordinate|queryname>" :default "coordinate"])]
    (when (:help opt)
      (exit-with 0 (println help)))
    (let [r (reader in)
          w (writer out)]
      (condp = (:order opt)
        (name sorter/order-coordinate) (sorter/sort-by-pos r w)
        (name sorter/order-queryname) (sorter/sort-by-qname r w)))
    nil))

(defn index [args]
  (let [[opt [f _] help] (cli args
                              "Usage: cljam index <in.bam>"
                              ["-h" "--help" "Print help" :default false :flag true])]
    (when (:help opt)
      (exit-with 0 (println help)))
    (bai/create-index f (str f ".bai"))
    nil))

(defn- pileup-with-ref
  [rdr ref-fa]
  (with-open [fa-rdr (fa/reader ref-fa)]
   (doseq [rname (map :name (read-refs rdr))
           line  (plp/mpileup rdr rname -1 -1 :ref-fasta fa-rdr)]
     (if-not (zero? (:count line))
       (println (clojure.string/join \tab (map val line)))))))

(defn- pileup-without-ref
  [rdr]
  (doseq [rname (map :name (read-refs rdr))
          line  (plp/mpileup rdr rname)]
    (if-not (zero? (:count line))
      (println (clojure.string/join \tab (map val line))))))

(defn pileup [args]
  (let [[opt [f _] help] (cli args
                              "Usage: cljam pileup <in.bam>"
                              ["-h" "--help" "Print help" :default false :flag true]
                              ["-r" "--ref" "Reference file in the FASTA format."])]
    (when (:help opt)
      (exit-with 0 (println help)))
    (with-open [r (reader f)]
      (when (= (type r) cljam.sam.reader.SAMReader)
        (exit-with 1 (println "Not support SAM file")))
      (when-not (sorter/sorted? r)
        (exit-with 1 (println "Not sorted")))
      (if (nil? (:ref opt))
        (pileup-without-ref r)
        (pileup-with-ref r (:ref opt))))
    nil))

(defn faidx [args]
  (let [[opt [f _] help] (cli args
                              "Usage: cljam faidx <ref.fasta>"
                              ["-h" "--help" "Print help" :default false :flag true])]
    (when (:help opt)
      (exit-with 0 (println help)))
    (fai/spit (str f ".fai")
              (fa/slurp f))
    nil))

(defn dict [args]
  (let [[opt [in out _] help] (cli args
                                   "Usage: cljam dict <ref.fasta> <out.dict>"
                                   ["-h" "--help" "Print help" :default false :flag true])]
    (when (:help opt)
      (exit-with 0 (println help)))
    (dict/create-dict in out)
    nil))

(defn -main [& args]
  (let [[opts cmd args help] (sub-command args
                                          "Usage: cljam {view,convert,sort,index,pileup,faidx,dict} ..."
                                          :options  [["-h" "--help" "Show help" :default false :flag true]]
                                          :commands [["view"    "Extract/print all or sub alignments in SAM or BAM format."]
                                                     ["convert" "Convert SAM to BAM or BAM to SAM."]
                                                     ["sort"    "Sort alignments by leftmost coordinates."]
                                                     ["index"   "Index sorted alignment for fast random access."]
                                                     ["pileup"  "Generate pileup for the BAM file."]
                                                     ["faidx"   "Index reference sequence in the FASTA format."]
                                                     ["dict"    "Create a FASTA sequence dictionary file."]])]
    (when (:help opts)
      (exit-with 0 (println help)))
    (case cmd
      :view    (view args)
      :convert (convert args)
      :sort    (sort args)
      :index   (index args)
      :pileup  (pileup args)
      :faidx   (faidx args)
      :dict    (dict args)
      (println help))))

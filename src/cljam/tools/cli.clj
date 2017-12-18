(ns cljam.tools.cli
  "Implementations of the command-line tool."
  (:refer-clojure :exclude [sort merge])
  (:require [clojure.string :as cstr]
            [clj-sub-command.core :refer [sub-command candidate-message]]
            [clojure.tools.cli :refer [parse-opts]]
            [cljam.io.sam :as sam]
            [cljam.io.sam.util :as sam-util]
            [cljam.io.sequence :as cseq]
            [cljam.algo.bam-indexer :as bai]
            [cljam.algo.normal :as normal]
            [cljam.algo.sorter :as sorter]
            [cljam.algo.fasta-indexer :as fai]
            [cljam.algo.dict :as dict]
            [cljam.algo.depth :as depth]
            [cljam.algo.pileup :as plp]
            [cljam.algo.convert :as convert]
            [cljam.algo.level :as level]
            [cljam.util.region :as region])
  (:import [java.io Closeable BufferedWriter OutputStreamWriter]))

;; CLI functions
;; -------------

(defn- exit
  "Exits the program with the status after printing the message."
  [status message]
  (binding [*out* (if (zero? status) *out* *err*)]
    (println message))
  (System/exit status))

(defn- error-msg
  "Returns error message strings from the errors."
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (cstr/join \newline errors)))

;; Sub-commands
;; ------------

;; ### view command

(def ^:private view-cli-options
  [[nil "--header" "Include header"]
   ["-f" "--format FORMAT" "Input file format <auto|sam|bam>"
    :default "auto"]
   ["-r" "--region REGION" "Only print in region (e.g. chr6:1000-2000)"]
   ["-h" "--help" "Print help"]])

(defn- view-usage [options-summary]
  (->> ["Extract/print all or sub alignments in SAM or BAM format."
        ""
        "Usage: cljam view [--header] [-f FORMAT] [-r REGION] <in.bam|sam>"
        ""
        "Options:"
        options-summary]
       (cstr/join \newline)))

(defn view [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args view-cli-options)]
    (cond
     (:help options) (exit 0 (view-usage summary))
     (not= (count arguments) 1) (exit 1 (view-usage summary))
     errors (exit 1 (error-msg errors)))
    (let [f (first arguments)]
      (with-open [^Closeable r (condp = (:format options)
                                 "auto" (sam/reader f)
                                 "sam"  (sam/sam-reader f)
                                 "bam"  (sam/bam-reader f))]
        (when (:header options)
          (println (sam-util/stringify-header (sam/read-header r))))
        (doseq [aln (if (:region options)
                      (if-let [region (region/parse-region (:region options))]
                        (if (sam/indexed? r)
                          (sam/read-alignments r region)
                          (exit 1 "Random alignment retrieval only works for indexed BAM."))
                        (exit 1 "Invalid region format"))
                      (sam/read-alignments r))]
          (println (sam-util/stringify-alignment aln))))))
  nil)

;; ### convert command

(def ^:private convert-cli-options
  [["-t" "--thread THREAD" "Number of threads (0 is auto)"
    :default 0
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help" "Print help"]])

(defn- convert-usage [options-summary]
  (->> ["Convert file format based on the file extension."
        ""
        "Usage: cljam convert [-t THREAD] <in-file> <out-file>"
        ""
        "Options:"
        options-summary]
       (cstr/join \newline)))

(defn convert [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args convert-cli-options)]
    (cond
     (:help options) (exit 0 (convert-usage summary))
     (not= (count arguments) 2) (exit 1 (convert-usage summary))
     errors (exit 1 (error-msg errors)))
    (let [[in out] arguments]
      (convert/convert in out :n-threads (:thread options))))
  nil)

;; ### normalize command

(def ^:private normalize-cli-options
  [["-h" "--help"]])

(defn- normalize-usage [options-summary]
  (->> ["Normalize references of alignments"
        ""
        "Usage: cljam normalize <in.bam|sam> <out.bam|sam>"
        ""
        "Options:"
        options-summary]
       (cstr/join \newline)))

(defn normalize [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args normalize-cli-options)]
    (cond
     (:help options) (exit 0 (normalize-usage summary))
     (not= (count arguments) 2) (exit 1 (normalize-usage summary))
     errors (exit 1 (error-msg errors)))
    (let [[in out] arguments]
      (normal/normalize-file! in out)))
  nil)

;; ### sort command

(def ^:private sort-cli-options
  [["-o" "--order ORDER" "Sorting order of alignments <coordinate|queryname>"
    :default "coordinate"]
   ["-c" "--chunk CHUNK" "Maximum number of alignments sorted per thread."
    :default sorter/default-chunk-size
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help" "Print help"]])

(defn- sort-usage [options-summary]
  (->> ["Sort alignments by leftmost coordinates."
        ""
        "Usage: cljam sort [-o ORDER] [-c CHUNK] <in.bam|sam> <out.bam|sam>"
        ""
        "Options:"
        options-summary]
       (cstr/join \newline)))

(defn sort [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args sort-cli-options)]
    (cond
     (:help options) (exit 0 (sort-usage summary))
     (not= (count arguments) 2) (exit 1 (sort-usage summary))
     errors (exit 1 (error-msg errors)))
    (let [[in out] arguments]
      (condp = (:order options)
        (name sorter/order-coordinate) (sorter/sort-by-pos in out {:chunk-size (:chunk options)})
        (name sorter/order-queryname) (sorter/sort-by-qname in out {:chunk-size (:chunk options)}))))
  nil)

;; ### index command

(def ^:private index-cli-options
  [["-t" "--thread THREAD" "Number of threads (0 is auto)"
    :default 0
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]])

(defn- index-usage [options-summary]
  (->> ["Index sorted alignment for fast random access."
        ""
        "Usage: cljam index [-t THREAD] <in.bam>"
        ""
        "Options:"
        options-summary]
       (cstr/join \newline)))

(defn index [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args index-cli-options)]
    (cond
     (:help options) (exit 0 (index-usage summary))
     (not= (count arguments) 1) (exit 1 (index-usage summary))
     errors (exit 1 (error-msg errors)))
    (let [f (first arguments)]
      (bai/create-index f (str f ".bai") :n-threads (:thread options))))
  nil)

;; ### pileup command

(def ^:private pileup-cli-options
  [["-s" "--simple" "Output only pileup count."]
   ["-r" "--region REGION" "Only pileup in region. (e.g. chr6:1000-2000)"]
   ["-t" "--thread THREAD" "Number of threads (0 is auto)"
    :default 0
    :parse-fn #(Integer/parseInt %)]
   ["-f" "--ref FASTA" "Reference file in the FASTA format."
    :default nil]
   ["-h" "--help"]])

(defn- pileup-usage [options-summary]
  (->> ["Generate pileup for the BAM file."
        ""
        "Usage: cljam pileup [-s] [-r REGION] [-f FASTA] [-t THREAD] <in.bam>"
        ""
        "Options:"
        options-summary]
       (cstr/join \newline)))

(defn- pileup-simple
  ([rdr n-threads]
   (doseq [rname (map :name (sam/read-refs rdr))]
     (pileup-simple rdr n-threads {:chr rname})))
  ([rdr n-threads region]
   (binding [*out* (BufferedWriter. (OutputStreamWriter. System/out))
             *flush-on-newline* false]
     (doseq [line (depth/lazy-depth rdr region {:n-threads n-threads})]
       (println line))
     (flush))))

(defn- pileup-with-ref
  ([rdr ref-fa]
   (with-open [fa-rdr (cseq/reader ref-fa)]
     (doseq [rname (map :name (sam/read-refs rdr))
             line  (plp/mpileup fa-rdr rdr {:chr rname})]
       (if-not (zero? (:count line))
         (println (cstr/join \tab (map #(case %
                                          :qual (cstr/join (% line))
                                          :seq (cstr/join (% line))
                                          (% line)) [:rname :pos :ref :count :seq :qual])))))))
  ([rdr ref-fa region]
   (with-open [fa-rdr (cseq/reader ref-fa)]
     (doseq [line  (plp/mpileup fa-rdr rdr region)]
       (if-not (zero? (:count line))
         (println (cstr/join \tab (map #(case %
                                          :qual (cstr/join (% line))
                                          :seq (cstr/join (% line))
                                          (% line)) [:rname :pos :ref :count :seq :qual]))))))))

(defn- pileup-without-ref
  ([rdr]
   (doseq [rname (map :name (sam/read-refs rdr))
           line  (plp/mpileup rdr {:chr rname})]
     (if-not (zero? (:count line))
       (println (cstr/join \tab (map #(case %
                                        :qual (cstr/join (% line))
                                        :seq (cstr/join (% line))
                                        (% line)) [:rname :pos :ref :count :seq :qual]))))))
  ([rdr region]
   (doseq [line  (plp/mpileup nil rdr region)]
     (if-not (zero? (:count line))
       (println (cstr/join \tab (map #(case %
                                        :qual (cstr/join (% line))
                                        :seq (cstr/join (% line))
                                        (% line)) [:rname :pos :ref :count :seq :qual])))))))

(defn pileup [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args pileup-cli-options)]
    (cond
     (:help options) (exit 0 (pileup-usage summary))
     (not= (count arguments) 1) (exit 1 (pileup-usage summary))
     errors (exit 1 (error-msg errors)))
    (let [f (first arguments)]
      (with-open [r (sam/reader f)]
        (when (= (type r) cljam.io.sam.reader.SAMReader)
          (exit 1 "Not support SAM file"))
        (when-not (sorter/sorted-by? r)
          (exit 1 "Not sorted"))
        (if (:region options)
          (if-let [region (region/parse-region (:region options))]
            (cond
              (:simple options) (pileup-simple r (:thread options) region)
              (:ref options) (pileup-with-ref r (:ref options) region)
              :else (pileup-without-ref r region))
            (exit 1 "Invalid region format"))
          (cond
            (:simple options) (pileup-simple r (:thread options))
            (:ref options) (pileup-with-ref r (:ref options))
            :else (pileup-without-ref r))))))
  nil)

;; ### faidx command

(def ^:private faidx-cli-options
  [["-h" "--help"]])

(defn- faidx-usage [options-summary]
  (->> ["Index reference sequence in the FASTA format."
        ""
        "Usage: cljam faidx <ref.fasta>"
        ""
        "Options:"
        options-summary]
       (cstr/join \newline)))

(defn faidx [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args faidx-cli-options)]
    (cond
     (:help options) (exit 0 (faidx-usage summary))
     (not= (count arguments) 1) (exit 1 (faidx-usage summary))
     errors (exit 1 (error-msg errors)))
    (let [f (first arguments)]
      (fai/create-index f (str f ".fai"))))
  nil)

;; ### dict command

(def ^:private dict-cli-options
  [["-h" "--help"]])

(defn- dict-usage [options-summary]
  (->> ["Create a FASTA sequence dictionary file."
        ""
        "Usage: cljam dict <ref.fasta> <out.dict>"
        ""
        "Options:"
        options-summary]
       (cstr/join \newline)))

(defn dict [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args dict-cli-options)]
    (cond
     (:help options) (exit 0 (dict-usage summary))
     (not= (count arguments) 2) (exit 1 (dict-usage summary))
     errors (exit 1 (error-msg errors)))
    (let [[in out] arguments]
      (dict/create-dict in out)))
  nil)

;; ### level command

(def ^:private level-cli-options
  [["-h" "--help"]])

(defn- level-usage [options-summary]
  (->> ["Analyze a BAM file and add level information of alignments."
        ""
        "Usage: cljam level <in.bam> <out.bam>"
        ""
        "Options:"
        options-summary]
       (cstr/join \newline)))

(defn level [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args level-cli-options)]
    (cond
      (:help options) (exit 0 (level-usage summary))
      (not= (count arguments) 2) (exit 1 (level-usage summary))
      errors (exit 1 (error-msg errors)))
    (let [[in out] arguments]
      (with-open [r (sam/reader in)
                  w (sam/writer out (sam/read-header r))]
        (level/add-level r w))))
  nil)

;; ### version command

(defn version [args]
  (let [ver (with-open [r (-> "META-INF/maven/cljam/cljam/pom.properties"
                              (clojure.java.io/resource)
                              (clojure.java.io/reader))]
              (.getProperty (doto (java.util.Properties.) (.load r)) "version"))]
    (exit 0 ver)))

;; Main command
;; ------------

(defn run [args]
  (let [[opts cmd args help cands]
        (sub-command args
                     "Usage: cljam {view,convert,normalize,sort,index,pileup,faidx,dict,level,version} ..."
                     :options  [["-h" "--help" "Show help" :default false :flag true]]
                     :commands [["view"    "Extract/print all or sub alignments in SAM or BAM format."]
                                ["convert" "Convert file format based on the file extension."]
                                ["normalize" "Normalize references of alignments."]
                                ["sort"    "Sort alignments by leftmost coordinates."]
                                ["index"   "Index sorted alignment for fast random access."]
                                ["pileup"  "Generate pileup for the BAM file."]
                                ["faidx"   "Index reference sequence in the FASTA format."]
                                ["dict"    "Create a FASTA sequence dictionary file."]
                                ["level"   "Add level of alignments."]
                                ["version" "Print version number."]])]
    (when (:help opts)
      (exit 0 help))
    (case cmd
      :view    (view args)
      :convert (convert args)
      :normalize (normalize args)
      :sort    (sort args)
      :index   (index args)
      :pileup  (pileup args)
      :faidx   (faidx args)
      :dict    (dict args)
      :level   (level args)
      :version (version args)
      (exit 1 (str "Invalid command. See 'cljam --help'."
                   (when (seq cands)
                     (str \newline (candidate-message cands))))))))

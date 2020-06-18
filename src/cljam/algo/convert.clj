(ns cljam.algo.convert
  "Converters between equivalent formats: SAM/BAM and FASTA/TwoBit."
  (:require [clojure.tools.logging :as logging]
            [clojure.string :as cstr]
            [cljam.common :refer [*n-threads* get-exec-n-threads]]
            [cljam.io.sam :as sam]
            [cljam.io.bam.encoder :as encoder]
            [cljam.io.sam.util.flag :as flag]
            [cljam.io.sam.util.refs :as refs]
            [cljam.util.sequence :as util-seq]
            [cljam.io.sequence :as cseq]
            [cljam.io.fastq :as fq]
            [cljam.io.util :as io-util]
            [com.climate.claypoole :as cp])
  (:import [java.nio ByteBuffer]
           [cljam.io.fastq FASTQRead]))

;;; SAM <-> BAM

(def ^:private default-num-block 100000)

(defn- sam-write-alignments [rdr wtr hdr num-block]
  (when (and (pos? *n-threads*) (> (get-exec-n-threads) 1))
    (logging/warn "Concurrent SAM writing is not supported."))
  (doseq [alns (partition-all num-block (sam/read-alignments rdr {}))]
    (sam/write-alignments wtr alns hdr)))

(defn- bam-write-alignments [rdr wtr hdr num-block]
  (let [refs (refs/make-refs hdr)
        n-threads (get-exec-n-threads)]
    (doseq [blocks (cp/pmap (if (= n-threads 1) :serial (dec n-threads))
                            (fn [chunk]
                              (mapv #(let [bb (ByteBuffer/allocate (encoder/get-block-size %))]
                                       (encoder/encode-alignment bb % refs)
                                       {:data (.array bb)})
                                    chunk))
                            (partition-all num-block (sam/read-alignments rdr {})))]
      (sam/write-blocks wtr blocks))))

(defn- convert-sam*
  [rdr wtr num-block write-alignments-fn]
  (let [hdr (sam/read-header rdr)]
    (sam/write-header wtr hdr)
    (sam/write-refs wtr hdr)
    (write-alignments-fn rdr wtr hdr num-block)))

(defn convert-sam
  "Converts file format between SAM and BAM based on the file extension."
  [in out & {:keys [n-threads num-block create-index?]
             :or {n-threads 0, num-block default-num-block, create-index? false}}]
  (with-open [rdr (sam/reader in)
              wtr (sam/writer out create-index?)]
    (binding [*n-threads* n-threads]
      (cond
        (io-util/sam-writer? wtr) (convert-sam* rdr wtr num-block sam-write-alignments)
        (io-util/bam-writer? wtr) (convert-sam* rdr wtr num-block bam-write-alignments)
        :else (throw (ex-info (str "Unsupported output file format " out) {})))))
  nil)

;;; FASTA <-> TwoBit

(defn convert-sequence
  "Converts file format between FASTA and TwoBit based on the file extension."
  ([in out]
   (convert-sequence identity in out))
  ([xf in out]
   (with-open [rdr (cseq/reader in)
               wtr (cseq/writer out {:index (if (cseq/indexed? rdr)
                                              (cseq/read-indices rdr)
                                              (logging/warn "Non-indexed sequence may use stupendous memory."))})]
     (cseq/write-sequences wtr (sequence xf (cseq/read-all-sequences rdr {:mask? true}))))))

;;; FASTQ -> FASTA or TwoBit

(defn fq->seq
  "Converts a FASTQ file to a FASTA or TwoBit sequence file."
  ([in out]
   (fq->seq identity in out))
  ([xf in out]
   (with-open [rdr (fq/reader in)
               wtr (cseq/writer out)]
     (->> (fq/read-sequences rdr {:decode-quality nil})
          (sequence xf)
          (cseq/write-sequences wtr)))))

;;; SAM -> FASTQ

(defn long-qname
  "Append casava 1.8 style R1/R2 suffix to the query name."
  [aln]
  (let [r (flag/r1r2 (:flag aln))]
    (if (zero? r)
      aln
      (update aln :qname str \space r ":N:0:1"))))

(defn medium-qname
  "Append _R1 or _R2 to the query name."
  [aln]
  (let [r (flag/r1r2 (:flag aln))]
    (if (zero? r)
      aln
      (update aln :qname str "_R" r))))

(defn short-qname
  "Append /1 or /2 to the query name."
  [aln]
  (let [r (flag/r1r2 (:flag aln))]
    (if (zero? r)
      aln
      (update aln :qname str \/ r))))

(defn- null-or-star? [^String qual]
  (or (nil? qual)
      (and (= 1 (.length qual))
           (= \* (.charAt qual 0)))))

(defn aln->read
  "Converts a SAM alignment record to a FASTQ read."
  [{:keys [flag ^String seq qual qname]}]
  (let [reversed? (flag/reversed? flag)]
    (FASTQRead.
     qname
     (if reversed? (util-seq/revcomp seq) seq)
     (if (null-or-star? qual)
       (cstr/join (repeat (.length seq) \"))
       (if reversed?
         (cstr/reverse qual)
         qual)))))

(defn- sam->fq-rf
  [w0 w1 w2 _ aln]
  (when-let [w (case (int (flag/r1r2 (:flag aln))) 0 w0 1 w1 2 w2)]
    (fq/write-sequences w [(aln->read aln)] {:encode-quality nil})))

(defn sam->fq
  "Converts a SAM/BAM to a FASTQ file."
  ([xf in out]
   (with-open [rdr (sam/reader in)
               wtr (fq/writer out)]
     (fq/write-sequences
      wtr
      (sequence
       (comp
        xf
        (filter (comp flag/primary? :flag))
        (map aln->read))
       (sam/read-alignments rdr))
      {:encode-quality nil})))
  ([xf in out-r1 out-r2]
   (with-open [rdr (sam/reader in)
               wtr1 (fq/writer out-r1)
               wtr2 (fq/writer out-r2)]
     (->> (sam/read-alignments rdr)
          (transduce
           (comp
            xf
            (filter (comp flag/primary? :flag)))
           (completing (partial sam->fq-rf nil wtr1 wtr2)) nil))))
  ([xf in out-r0 out-r1 out-r2]
   (with-open [rdr (sam/reader in)
               wtr0 (fq/writer out-r0)
               wtr1 (fq/writer out-r1)
               wtr2 (fq/writer out-r2)]
     (->> (sam/read-alignments rdr)
          (transduce
           (comp
            xf
            (filter (comp flag/primary? :flag)))
           (completing (partial sam->fq-rf wtr0 wtr1 wtr2)) nil)))))

;;; General converter

(defn- file-type [f] (when f (io-util/file-type f)))

(def ^:private alignment-io? (every-pred (comp #{:sam :bam} file-type)))

(def ^:private sequence-io? (every-pred (comp #{:fasta :2bit} file-type)))

(def ^:private read-io? (every-pred (comp #{:fastq} file-type)))

(defn convert
  "Converts file format from input file to output file by the file extension."
  [in out & opts]
  (cond
    (and (alignment-io? in) (sequential? out) (apply read-io? out)) (apply sam->fq (map short-qname) in out)
    (and (alignment-io? in) (not (sequential? out)) (read-io? out)) (sam->fq (map short-qname) in out)
    (alignment-io? in out) (apply convert-sam in out opts)
    (sequence-io? in out) (convert-sequence in out)
    (and (read-io? in) (sequence-io? out)) (fq->seq in out)
    :else (throw (ex-info (str "Unsupported I/O pair: " in " and " out) {}))))

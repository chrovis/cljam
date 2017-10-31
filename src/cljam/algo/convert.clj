(ns cljam.algo.convert
  "Converters between equivalent formats: SAM/BAM and FASTA/TwoBit."
  (:require [clojure.tools.logging :as logging]
            [cljam.common :refer [*n-threads* get-exec-n-threads]]
            [cljam.io.sam :as sam]
            [cljam.io.bam.encoder :as encoder]
            [cljam.io.sam.util :as sam-util]
            [cljam.io.sequence :as cseq]
            [cljam.io.util :as io-util]
            [com.climate.claypoole :as cp])
  (:import [java.nio ByteBuffer]
           [cljam.io.bam.writer BAMWriter]))

;;; SAM <-> BAM

(def ^:private default-num-block 100000)

(defn- sam-write-alignments [rdr wtr hdr num-block]
  (when (and (pos? *n-threads*) (> (get-exec-n-threads) 1))
    (logging/warn "Concurrent SAM writing is not supported."))
  (doseq [alns (partition-all num-block (sam/read-alignments rdr {}))]
    (sam/write-alignments wtr alns hdr)))

(defn- bam-write-alignments [rdr wtr hdr num-block]
  (let [refs (sam-util/make-refs hdr)
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
  [in out & {:keys [n-threads num-block]
             :or {n-threads 0, num-block default-num-block}}]
  (with-open [rdr (sam/reader in)
              wtr (sam/writer out)]
    (binding [*n-threads* n-threads]
      (cond
        (io-util/sam-writer? wtr) (convert-sam* rdr wtr num-block sam-write-alignments)
        (io-util/bam-writer? wtr) (convert-sam* rdr wtr num-block bam-write-alignments)
        :else (throw (ex-info (str "Unsupported output file format " out) {})))))
  nil)

;;; FASTA <-> TwoBit

(defn convert-sequence
  "Converts file format between FASTA and TwoBit based on the file extension."
  [in out]
  (with-open [rdr (cseq/reader in)
              wtr (cseq/writer out {:index (if (cseq/indexed? rdr)
                                             (cseq/read-indices rdr)
                                             (logging/warn "Non-indexed sequence may use stupendous memory."))})]
    (cseq/write-sequences wtr (cseq/read-all-sequences rdr {:mask? true}))))

;;; General converter

(def ^:private alignment-io? (every-pred (comp #{:sam :bam} io-util/file-type)))

(def ^:private sequence-io? (every-pred (comp #{:fasta :2bit} io-util/file-type)))

(defn convert
  "Converts file format from input file to output file by the file extension."
  [in out & opts]
  (cond
    (alignment-io? in out) (apply convert-sam in out opts)
    (sequence-io? in out) (convert-sequence in out)
    :else (throw (ex-info (str "Unsupported I/O pair: " in " and " out) {}))))

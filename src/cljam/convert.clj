(ns cljam.convert
  "Convert file from sam to bam, and vice versa"
  (:require [cljam.core :as core]
            [cljam.io :as io]
            [cljam.bam.encoder :as encoder]
            [cljam.util.sam-util :as sam-util]
            [com.climate.claypoole :as cp]))

(def ^:private default-num-block 10000)
(def ^:private default-num-write-block 1000)

(defn- _convert!
  [in out num-block num-write-block alignments-writer]
  (let [num-block (or num-block default-num-block)
        num-write-block (or num-write-block default-num-write-block)]
    (with-open [wtr (core/writer out)]
      (with-open [rdr (core/reader in)]
        (let [hdr (io/read-header rdr)]
          (io/write-header wtr hdr)
          (io/write-refs wtr hdr)
          (alignments-writer rdr wtr hdr num-block num-write-block))))))

(defn- sam-alignments-writer [rdr wtr hdr num-block num-write-block]
  (doseq [alns (partition-all num-block (io/read-alignments rdr {}))]
    (io/write-alignments wtr alns hdr)))

(defn convert-bam-to-sam
  "Read from input bam file, and write to output sam file"
  [in out & [num-block num-write-block]]
  (_convert! in out num-block num-write-block sam-alignments-writer))

(defn- bam-alignments-writer [rdr wtr hdr num-block num-write-block]
  (let [refs (sam-util/make-refs hdr)
        w (.writer wtr)]
    (cp/with-shutdown! [pool (cp/threadpool (cp/ncpus))]
      (doseq [alns (partition-all num-block (io/read-alignments rdr {}))]
        (let [blocks (doall (cp/pmap pool
                                     (fn [lalns]
                                       (doall (map #(encoder/encode % refs)
                                                   lalns)))
                                     (partition-all num-write-block alns)))]
          (doseq [block blocks]
            (doseq [e block]
              (encoder/write-encoded-alignment w e))))))))

(defn convert-sam-to-bam
  "Read from input sam file, and write to output bam file"
  [in out & [num-block num-write-block]]
  (_convert! in out num-block num-write-block bam-alignments-writer))

(defn convert
  "Convert file format from input file to output file by file extension"
  [in out & [num-block num-write-block]]
  (condp re-find out
    #"\.sam$" (convert-bam-to-sam in out num-block num-write-block)
    #"\.bam$" (convert-sam-to-bam in out num-block num-write-block)
    (throw (ex-info (str "Unsupported output file format " out) {})))
  nil)

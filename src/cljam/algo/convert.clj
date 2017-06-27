(ns cljam.algo.convert
  "Format converter from SAM to BAM, and vice versa."
  (:require [cljam.io.sam :as sam]
            [cljam.io.bam.encoder :as encoder]
            [cljam.io.sam.util :as sam-util]
            [cljam.io.util :as io-util]
            [com.climate.claypoole :as cp])
  (:import [cljam.io.bam.writer BAMWriter]))

(def ^:private default-num-block 100000)
(def ^:private default-num-write-block 10000)

(defn- sam-write-alignments [rdr wtr hdr num-block num-write-block]
  (doseq [alns (partition-all num-block (sam/read-alignments rdr {}))]
    (sam/write-alignments wtr alns hdr)))

(defn- bam-write-alignments [rdr wtr hdr num-block num-write-block]
  (let [refs (sam-util/make-refs hdr)
        w (.writer ^BAMWriter wtr)]
    (cp/with-shutdown! [pool (cp/threadpool (cp/ncpus))]
      (doseq [alns (partition-all num-block (sam/read-alignments rdr {}))]
        (let [blocks (doall (cp/pmap pool
                                     (fn [lalns]
                                       (doall (map #(encoder/encode % refs)
                                                   lalns)))
                                     (partition-all num-write-block alns)))]
          (doseq [block blocks]
            (doseq [e block]
              (encoder/write-encoded-alignment w e))))))))

(defn- _convert!
  [rdr wtr num-block num-write-block write-alignments-fn]
  (let [hdr (sam/read-header rdr)]
    (sam/write-header wtr hdr)
    (sam/write-refs wtr hdr)
    (write-alignments-fn rdr wtr hdr num-block num-write-block)))

(defn convert
  "Converts file format from input file to output file by the file extension."
  [in out & {:keys [num-block num-write-block]
             :or {num-block default-num-block
                  num-write-block default-num-write-block}}]
  (with-open [rdr (sam/reader in)
              wtr (sam/writer out)]
    (cond
      (io-util/sam-writer? wtr) (_convert! rdr wtr num-block num-write-block sam-write-alignments)
      (io-util/bam-writer? wtr) (_convert! rdr wtr num-block num-write-block bam-write-alignments)
      :else (throw (ex-info (str "Unsupported output file format " out) {}))))
  nil)

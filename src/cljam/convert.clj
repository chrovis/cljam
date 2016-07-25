(ns cljam.convert
  "Convert file from sam to bam, and vice versa"
  (:require [cljam.core :as core]
            [cljam.io :as io]
            [cljam.bam.encoder :as encoder]
            [cljam.util.sam-util :as sam-util]
            [com.climate.claypoole :as cp]))

(def ^:private default-num-block 100000)
(def ^:private default-num-write-block 10000)

(defn- sam-write-alignments [rdr wtr hdr num-block num-write-block]
  (doseq [alns (partition-all num-block (io/read-alignments rdr {}))]
    (io/write-alignments wtr alns hdr)))

(defn- bam-write-alignments [rdr wtr hdr num-block num-write-block]
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

(defn- _convert!
  [rdr wtr num-block num-write-block write-alignments-fn]
  (let [hdr (io/read-header rdr)]
    (io/write-header wtr hdr)
    (io/write-refs wtr hdr)
    (write-alignments-fn rdr wtr hdr num-block num-write-block)))

(defn convert
  "Convert file format from input file to output file by file extension"
  [in out & {:keys [num-block num-write-block]
             :or {num-block default-num-block
                  num-write-block default-num-write-block}}]
  (with-open [rdr (core/reader in)
              wtr (core/writer out)]
    (cond
      (core/sam-writer? wtr) (_convert! rdr wtr num-block num-write-block sam-write-alignments)
      (core/bam-writer? wtr) (_convert! rdr wtr num-block num-write-block bam-write-alignments)
      :else (throw (ex-info (str "Unsupported output file format " out) {}))))
  nil)

(ns cljam.algo.normal
  "Functions to normalize the SAM/BAM format."
  (:require [cljam.io.sam :as sam]
            [cljam.io.util :as io-util]
            [cljam.util.chromosome :refer [normalize-chromosome-key]]))

(def ^:private chunk-size 1500000)

(defn- normalize-header
  [hdr]
  (update hdr :SQ (fn [xs]
                    (mapv #(update % :SN normalize-chromosome-key) xs))))

;; TODO: copy all rest of stream for performance. (do not read, parse and write)
(defn- transfer-blocks
  [rdr wtr]
  (doseq [blks (partition-all chunk-size (sam/read-blocks rdr))]
    (sam/write-blocks wtr blks)))

(defn- transfer-alignments
  [rdr wtr]
  (doseq [alns (->> (sam/read-alignments rdr)
                    (map #(update % :rname normalize-chromosome-key))
                    (partition-all chunk-size))]
    (sam/write-alignments wtr alns)))

(defn normalize
  "Normalizes references of the SAM/BAM format. Be noted that performance may be
  degraded if either or both of rdr and wtr is one about the SAM format."
  [rdr wtr]
  (let [hdr (normalize-header (sam/read-header rdr))]
    (if (and (io-util/bam-reader? rdr) (io-util/bam-writer? wtr))
      (transfer-blocks rdr wtr)
      (transfer-alignments rdr wtr))))

(defn normalize-file!
  "Normalizes references of the SAM/BAM format. Be noted that performance may be
  degraded if either or both of in-file and out-file are SAM."
  [in out]
  (with-open [rdr (sam/reader in)
              wtr (sam/writer out (normalize-header (sam/read-header rdr)))]
    (normalize rdr wtr)))

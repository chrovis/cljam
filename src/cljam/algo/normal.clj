(ns cljam.algo.normal
  (:require [cljam.io.sam :as sam]
            [cljam.io.util :as io-util]
            [cljam.util :refer [swap]]
            [cljam.util.chromosome :refer [normalize-chromosome-key]]))

(def ^:private chunk-size 1500000)

(defn- normalize-header
  [hdr]
  (let [seq (:SQ hdr)]
    (assoc hdr
      :SQ (vec (map #(swap % :SN normalize-chromosome-key) seq)))))

;; TODO: copy all rest of stream for performance. (do not read, parse and write)
(defn- transfer-blocks
  [rdr wtr]
  (doseq [blks (partition-all chunk-size (sam/read-blocks rdr))]
    (sam/write-blocks wtr blks)))

(defn- transfer-alignments
  [rdr wtr hdr]
  (doseq [alns (->> (sam/read-alignments rdr)
                    (map #(update % :rname normalize-chromosome-key))
                    (partition-all chunk-size))]
    (sam/write-alignments wtr alns hdr)))

(defn normalize
  "Normalizes references of the SAM/BAM format. Be noted that performance may be
  degraded if either or both of rdr and wtr is one about the SAM format."
  [rdr wtr]
  (let [hdr (normalize-header (sam/read-header rdr))]
    (sam/write-header wtr hdr)
    (sam/write-refs wtr hdr)
    (if (and (io-util/bam-reader? rdr) (io-util/bam-writer? wtr))
      (transfer-blocks rdr wtr)
      (transfer-alignments rdr wtr hdr))))

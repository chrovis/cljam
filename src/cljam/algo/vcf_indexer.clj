(ns cljam.algo.vcf-indexer
  "Indexer of VCF/BCF."
  (:require [cljam.io.csi :as csi]
            [cljam.io.util :as io-util]
            [cljam.io.vcf :as vcf]))

(defn create-index
  "Creates a CSI index file from the VCF/BCF file."
  [variant-file out-csi {:keys [shift depth] :or {shift 14 depth 5}}]
  (with-open [r (vcf/reader variant-file)]
    (->> {:variant-file-type (if (io-util/vcf-reader? r) :vcf :bcf)}
         (csi/offsets->index (vcf/read-file-offsets r) shift depth)
         (csi/write-index out-csi))))

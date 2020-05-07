(ns cljam.algo.bcf-indexer
  "Indexer of BCF."
  (:require [cljam.io.csi :as csi]
            [cljam.io.vcf :as vcf])
  (:import cljam.io.bcf.reader.BCFReader))

(defn create-index
  "Creates a CSI index file from the BCF file."
  [in-bcf out-csi {:keys [shift depth] :or {shift 14 depth 5}}]
  (with-open [r (vcf/reader in-bcf)]
    (let [offsets (vcf/read-file-offsets r)
          names (->> (.meta-info ^BCFReader r)
                     :contig
                     (mapv :id))
          csi (csi/offsets->index offsets shift depth
                                  {:variant-file-type :bcf
                                   :names names})]
      (csi/write-index out-csi csi))))

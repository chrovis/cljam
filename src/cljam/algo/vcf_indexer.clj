(ns cljam.algo.vcf-indexer
  "Indexer of VCF."
  (:require [cljam.io.csi :as csi]
            [cljam.io.vcf :as vcf])
  (:import cljam.io.vcf.reader.VCFReader))

(defn create-index
  "Creates a CSI index file from the VCF file."
  [in-bgzipped-vcf out-csi {:keys [shift depth] :or {shift 14 depth 5}}]
  (with-open [r (vcf/reader in-bgzipped-vcf)]
    (let [offsets (vcf/read-file-offsets r)

          names (->> (map :chr offsets)
                     (concat (->> (.meta-info ^VCFReader r)
                                  :contig
                                  (mapv :id)))
                     distinct)
          csi (csi/offsets->index offsets shift depth
                                  {:variant-file-type :vcf
                                   :names names})]
      (csi/write-index out-csi csi))))

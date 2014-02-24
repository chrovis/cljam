(ns cljam.bam-indexer
  (:require [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.bam-index.writer :as bai-writer]))

(defn create-index!
  "Creates a BAM index file from the BAM file."
  [in-bam out-bai]
  (with-open [r (bam/reader in-bam :ignore-index true)
              w (bai-writer/writer out-bai (io/read-refs r))]
    (bai-writer/write-index w
                            (io/read-alignments r {:depth :pointer}))))

(def ^:deprecated create-index create-index!)

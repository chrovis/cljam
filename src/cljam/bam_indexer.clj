(ns cljam.bam-indexer
  "Indexer of BAM."
  (:require [cljam.io :as io]
            [cljam.bam :as bam]
            [cljam.bam-index.core :as bai-core]))

(defn create-index
  "Creates a BAM index file from the BAM file."
  [in-bam out-bai]
  (with-open [r ^cljam.bam.reader.BAMReader (bam/reader in-bam :ignore-index true)]
    (bai-core/create-index out-bai
                           (io/read-alignments r {:depth :pointer})
                           (io/read-refs r))))

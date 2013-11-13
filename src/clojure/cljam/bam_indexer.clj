(ns cljam.bam-indexer
  (:require [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.bam-indexer.writer :as bai-writer]))

(defn create-index
  [in-bam out-bai]
  (with-open [r (bam/reader in-bam)
              w (bai-writer/writer out-bai (io/read-refs r))]
    (bai-writer/write-index w
                            (io/read-alignments r {:depth :pointer}))))

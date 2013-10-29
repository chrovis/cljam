(ns cljam.bam-indexer
  (:require [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.bam-indexer.writer :as bai-writer]))

(defn writer
  [f]
  (bai-writer/writer f))

(defn create-index
  [in-bam out-bai]
  (with-open [r (bam/reader in-bam)
              w (writer out-bai)]
    (io/write-bam-index w (io/read-refs r) (io/read-alignments r {:depth :full}))))

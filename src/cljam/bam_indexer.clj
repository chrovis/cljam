(ns cljam.bam-indexer
  "Indexer of BAM."
  (:require [clojure.tools.logging :as logging]
            [me.raynes.fs :as fs]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.bam-index.writer :as bai-writer]))

(defn create-index
  "Creates a BAM index file from the BAM file."
  [in-bam out-bai]
  (with-open [r (bam/reader in-bam :ignore-index true)
              w (bai-writer/writer out-bai (io/read-refs r))]
    (try
      (bai-writer/write-index! w (io/read-alignments r {:depth :pointer}))
      (catch Exception e (do
                           (fs/delete (.f w))
                           (logging/error "Failed to create BAM index")
                           (throw e))))))

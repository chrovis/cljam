(ns cljam.algo.bam-indexer
  "Indexer of BAM."
  (:require [cljam.common :refer [*n-threads*]]
            [cljam.io :as io]
            [cljam.io.bam :as bam]
            [cljam.io.bam-index.core :as bai-core]))

(defn create-index
  "Creates a BAM index file from the BAM file."
  [in-bam out-bai & {:keys [n-threads] :or {n-threads 0}}]
  (with-open [r (bam/reader in-bam :ignore-index true)]
    (binding [*n-threads* n-threads]
      (bai-core/create-index out-bai
                             (io/read-blocks r {} {:mode :pointer})
                             (io/read-refs r)))))

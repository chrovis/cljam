(ns cljam.algo.bam-indexer
  "Indexer of BAM."
  (:require [cljam.common :refer [*n-threads*]]
            [cljam.io.sam :as sam]
            [cljam.io.bam-index.core :as bai-core]))

(defn create-index
  "Creates a BAM index file from the BAM file."
  [in-bam out-bai & {:keys [n-threads] :or {n-threads 0}}]
  (with-open [r (sam/bam-reader in-bam)]
    (binding [*n-threads* n-threads]
      (bai-core/create-index out-bai (sam/read-blocks r) (sam/read-refs r)))))

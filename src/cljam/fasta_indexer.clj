(ns cljam.fasta-indexer
  "Alpha - subject to change.
  Indexer of FASTA."
  (:require [cljam.fasta :as fasta]
            [cljam.fasta-index.core :as fai-core]))

(defn create-index
  "Create a FASTA index file from the FASTA file."
  [in-fa out-fai]
  (with-open [r (fasta/reader in-fa)]
    (fai-core/create-index out-fai
                           (.headers r)
                           (fasta/read-sequences r))))

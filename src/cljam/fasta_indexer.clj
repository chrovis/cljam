(ns cljam.fasta-indexer
  "Alpha - subject to change.
  Indexer of FASTA."
  (:require [cljam.fasta :as fasta]
            [cljam.fasta-index.writer :as fai-writer]))

(defn create-index!
  "Create a FASTA index file from the FASTA file."
  [fa fai]
  (with-open [r (fasta/reader fa)
              w (fai-writer/writer fai)]
    (fai-writer/write-index! w (fasta/read r))))

(def ^:deprecated create-index create-index!)

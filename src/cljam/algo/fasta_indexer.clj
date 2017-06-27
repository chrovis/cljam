(ns cljam.algo.fasta-indexer
  "Indexer of FASTA."
  (:require [cljam.io.fasta-index.core :as fai-core]))

(defn create-index
  "Creates a FASTA index file from the FASTA file."
  [in-fa out-fai]
  (fai-core/create-index in-fa out-fai))

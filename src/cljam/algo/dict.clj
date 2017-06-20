(ns cljam.algo.dict
  "Alpha - subject to change.
  Generator of a FASTA sequence dictionary file."
  (:require [clojure.java.io :as cio]
            [cljam.io.fasta :as fasta]
            [cljam.io.dict.core :as dict-core]))

(defn create-dict
  "Creates a FASTA sequence dictionary file (.dict) from the specified FASTA
  file. The unfinished file will be deleted when failing."
  [fasta out-dict]
  (with-open [r (fasta/reader fasta :ignore-index true)]
    (dict-core/create-dict out-dict
                           (fasta/read-headers r)
                           (fasta/read-sequences r)
                           (str (.. (cio/file fasta) getCanonicalFile toURI)))))

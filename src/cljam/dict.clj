(ns cljam.dict
  "Alpha - subject to change.
  Generator of a FASTA sequence dictionary file."
  (:require [clojure.java.io :as io]
            [cljam.fasta :as fasta]
            [cljam.dict.core :as dict-core]))

(defn create-dict
  "Creates a FASTA sequence dictionary file (.dict) from the specified FASTA
  file. The unfinished file will be deleted when failing."
  [fasta out-dict]
  (with-open [r (fasta/reader fasta)]
    (dict-core/create-dict out-dict
                           (fasta/read r)
                           (str (.. (io/file fasta) getCanonicalFile toURI)))))

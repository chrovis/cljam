(ns cljam.algo.dict
  "Alpha - subject to change.
  Generator of a FASTA sequence dictionary file."
  (:require [clojure.java.io :as cio]
            [cljam.io.dict.core :as dict-core]
            [cljam.io.fasta.core :as fa-core]
            [cljam.io.sequence :as cseq]))

(defn create-dict
  "Creates a FASTA sequence dictionary file (.dict) from the specified FASTA
  file. The unfinished file will be deleted when failing."
  [fasta out-dict]
  (with-open [r (cseq/fasta-reader fasta)]
    (dict-core/create-dict out-dict
                           (fa-core/read-headers r)
                           (fa-core/read-sequences r)
                           (str (.. (cio/file fasta) getCanonicalFile toURI)))))

(ns cljam.core
  "Core features of cljam."
  (:require [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.fasta :as fasta]))

(defn reader
  "Selects suitable reader from f's extension, returning the reader. This
  function supports SAM and BAM format."
  [f & {:keys [ignore-index] :or {ignore-index true}}]
  (condp re-find f
    #"\.sam$" (sam/reader f)
    #"\.bam$" (bam/reader f :ignore-index ignore-index)
    #"\.fa" (fasta/reader f)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn writer
  "Selects suitable writer from f's extension, returning the writer. This
  function supports SAM and BAM format."
  [f]
  (condp re-find f
    #"\.sam$" (sam/writer f)
    #"\.bam$" (bam/writer f)
    (throw (IllegalArgumentException. "Invalid file type"))))

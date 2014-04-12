(ns cljam.fasta-index.core
  "The core of FASTA index features."
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as logging]
            [me.raynes.fs :as fs]
            [cljam.fasta-index.writer :as writer]))

;; Writing
;; -------

(defn writer
  [f]
  (cljam.fasta_index.writer.FAIWriter.
   (io/writer f)
   (.getAbsolutePath (io/file f))))

(defn create-index
  "Creates a FASTA index file from the sequences."
  [f headers sequences]
  (with-open [w ^cljam.fasta_index.writer.FAIWriter (writer f)]
    (try
      (writer/write-index! w headers sequences)
      (catch Exception e (do
                           (fs/delete (.f w))
                           (logging/error "Failed to create FASTA index")
                           (throw e))))))

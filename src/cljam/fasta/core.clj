(ns cljam.fasta.core
  (:refer-clojure :exclude [read slurp])
  (:require [clojure.java.io :as io]
            [cljam.fasta.reader :as reader])
  (:import java.io.RandomAccessFile))

;; Reading
;; -------

(defn reader
  [f]
  (cljam.fasta.reader.FASTAReader.
   (RandomAccessFile. f "r")
   (.getAbsolutePath (io/file f))))

(defn read
  "Reads FASTA sequence data, returning its information as a lazy sequence."
  [rdr]
  (reader/read rdr))

(defn slurp
  "Opens a reader on a FASTA file and reads all its contents, returning
  a sequence about the data."
  [f]
  (with-open [r (reader f)]
    (doall (reader/read r))))

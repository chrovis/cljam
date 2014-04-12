(ns cljam.fasta.core
  (:refer-clojure :exclude [read slurp])
  (:require [clojure.java.io :as io]
            [cljam.fasta.reader :as reader])
  (:import java.io.RandomAccessFile))

;; Reading
;; -------

(defn reader
  [^String f]
  (let [headers (with-open [r (RandomAccessFile. f "r")]
                  (reader/load-headers r))]
    (cljam.fasta.reader.FASTAReader. (RandomAccessFile. f "r")
                                     (.getAbsolutePath (io/file f))
                                     headers)))

(defn read-headers
  [^cljam.fasta.reader.FASTAReader rdr]
  (.headers rdr))

(defn read-sequences
  "Reads sequences by line, returning the line-separated sequences
  as lazy sequence."
  [rdr]
  (reader/read-sequences rdr))

(defn read
  [rdr]
  (reader/read rdr))

(defn slurp
  "Opens a reader on a FASTA file and reads all its contents, returning
  a sequence about the data."
  [f]
  (with-open [r (reader f)]
    (doall (reader/read r))))

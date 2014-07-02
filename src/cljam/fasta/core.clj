(ns cljam.fasta.core
  (:refer-clojure :exclude [read slurp])
  (:require [clojure.java.io :as io]
            [cljam.fasta-index.core :as fasta-index]
            [cljam.fasta.reader :as reader])
  (:import java.io.RandomAccessFile
           cljam.fasta.reader.FASTAReader))

;; Reading
;; -------

(defn ^FASTAReader reader
  [^String f]
  (let [f (.getAbsolutePath (io/file f))
        index-f (str f ".fai")
        index (when (.exists (io/file index-f))
                (fasta-index/reader index-f))]
    (FASTAReader. (RandomAccessFile. f "r")
                  f
                  index)))

(defn read-headers
  [^FASTAReader rdr]
  (if (.index rdr)
    (fasta-index/get-headers (.index rdr))
    (reader/load-headers (.reader rdr))))

(defn read-indices
  [^FASTAReader rdr]
  (when (.index rdr)
    (fasta-index/get-indices (.index rdr))))

(defn read-sequences
  "Reads sequences by line, returning the line-separated sequences
  as lazy sequence."
  [rdr]
  (reader/read-sequences rdr))

(defn read-sequence
  ([rdr name]
     (reader/read-whole-sequence rdr name))
  ([rdr name start end]
     (reader/read-sequence rdr name start end)))

(defn read
  [rdr]
  (reader/read rdr))

(defn slurp
  "Opens a reader on a FASTA file and reads all its contents, returning
  a sequence about the data."
  [f]
  (with-open [r (reader f)]
    (doall (reader/read r))))

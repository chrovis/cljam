(ns cljam.fasta.core
  (:refer-clojure :exclude [read slurp])
  (:require [clojure.java.io :as io]
            [cljam.util :as util]
            [cljam.fasta-index.core :as fasta-index]
            [cljam.fasta.reader :as reader])
  (:import java.io.RandomAccessFile
           cljam.fasta.reader.FASTAReader))

;; Reading
;; -------

(defn ^FASTAReader reader
  [^String f {:keys [ignore-index]
              :or {ignore-index false}}]
  (let [f (.getAbsolutePath (io/file f))
        index-f (str f ".fai")
        index (if-not ignore-index
                (if (.exists (io/file index-f))
                  (fasta-index/reader index-f)
                  (throw (java.io.FileNotFoundException.
                          (str index-f " (No such FASTA index)")))))]
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

(defn reset
  [rdr]
  (reader/reset rdr))

(defn slurp
  "Opens a reader on a FASTA file and reads all its contents, returning
  a sequence about the data."
  [f]
  (with-open [r (reader f)]
    (doall (reader/read r))))

(defn sequential-read-byte-array
  [f]
  (with-open [stream (util/compressor-input-stream f)]
    (reader/sequential-read-byte-array stream (* 1024 1024 10) 536870912)))

(defn sequential-read
  [f]
  (with-open [stream (util/compressor-input-stream f)]
    (reader/sequential-read-string stream (* 1024 1024 10) 536870912)))

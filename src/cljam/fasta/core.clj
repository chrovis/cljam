(ns cljam.fasta.core
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as cio]
            [cljam.io :as io]
            [cljam.util :as util]
            [cljam.fasta-index.core :as fasta-index]
            [cljam.fasta.reader :as reader])
  (:import [java.io RandomAccessFile]
           [cljam.fasta.reader FASTAReader]))

;; Reading
;; -------

(defn ^FASTAReader reader
  [^String f {:keys [ignore-index]
              :or {ignore-index false}}]
  (let [f (.getAbsolutePath (cio/file f))
        index-f (str f ".fai")
        index (if-not ignore-index
                (if (.exists (cio/file index-f))
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

(defn sequential-read
  [f]
  (with-open [stream (util/compressor-input-stream f)]
    (reader/sequential-read-string stream (* 1024 1024 10) 536870912)))

(extend-type FASTAReader
  io/IReader
  (reader-path [this] (.f this))
  (read
    ([this] (sequential-read (.f this)))
    ([this option] (sequential-read (.f this))))
  io/IRegionReader
  (read-in-region
    ([this region]
     (io/read-in-region this region {}))
    ([this region option]
     (io/read-sequence this region option)))
  io/ISequenceReader
  (read-sequence
    ([this region]
     (io/read-sequence this region {}))
    ([this {:keys [chr start end]} option]
     (reader/read-sequence this chr start end))))

(ns cljam.io.fasta.core
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as cio]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util]
            [cljam.io.fasta-index.core :as fasta-index]
            [cljam.io.fasta.reader :as reader])
  (:import [java.io RandomAccessFile]
           [cljam.io.fasta.reader FASTAReader]))

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
  [rdr {:keys [chr start end]}]
  (if (and (nil? start) (nil? end))
    (reader/read-whole-sequence rdr chr)
    (reader/read-sequence rdr chr start end)))

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
  protocols/IReader
  (reader-path [this] (.f this))
  (read
    ([this] (protocols/read this {}))
    ([this option] (protocols/read-all-sequences this option)))
  protocols/IRegionReader
  (read-in-region
    ([this region]
     (protocols/read-in-region this region {}))
    ([this region option]
     (protocols/read-sequence this region option)))
  protocols/ISequenceReader
  (read-all-sequences
    ([this] (protocols/read-all-sequences this {}))
    ([this option]
     (sequential-read (.f this))))
  (read-sequence
    ([this region]
     (protocols/read-sequence this region {}))
    ([this region option]
     (read-sequence this region))))

(ns cljam.io.fasta.core
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util]
            [cljam.io.fasta-index.core :as fai]
            [cljam.io.fasta.reader :as reader])
  (:import [java.io FileNotFoundException RandomAccessFile]
           [cljam.io.fasta.reader FASTAReader]))

;; Reading
;; -------

(defn- fasta-index
  [fasta-path]
  (if-let [fai-path (->> ["$1.fai" ".fai" "$1.FAI" ".FAI"]
                         (eduction
                          (comp
                           (map #(cstr/replace fasta-path #"(?i)(\.fa(sta)?)$" %))
                           (filter #(.isFile (cio/file %)))))
                         first)]
    (fai/reader fai-path)
    (throw (FileNotFoundException.
            (str "Could not find FASTA Index file for " fasta-path)))))

(defn ^FASTAReader reader
  [f]
  (let [f (.getAbsolutePath (cio/file f))]
    (FASTAReader. (RandomAccessFile. f "r")
                  f
                  (delay (fasta-index f)))))

(defn ^FASTAReader clone-reader
  "Clones fasta reader sharing persistent objects."
  [^FASTAReader rdr]
  (let [f (.f rdr)
        raf (RandomAccessFile. f "r")]
    (FASTAReader. raf f (.index-delay rdr))))

(defn read-headers
  [^FASTAReader rdr]
  (try
    (fai/get-headers @(.index-delay rdr))
    (catch FileNotFoundException _
      (reader/load-headers (.reader rdr)))))

(defn read-indices
  [^FASTAReader rdr]
  (fai/get-indices @(.index-delay rdr)))

(defn read-sequences
  "Reads sequences by line, returning the line-separated sequences
  as lazy sequence."
  [rdr]
  (reader/read-sequences rdr))

(defn read-sequence
  [rdr {:keys [chr start end]} opts]
  (if (and (nil? start) (nil? end))
    (reader/read-whole-sequence rdr chr opts)
    (reader/read-sequence rdr chr start end opts)))

(defn read
  [rdr]
  (reader/read rdr))

(defn reset
  [rdr]
  (reader/reset rdr))

(defn sequential-read
  ([f]
   (sequential-read f {}))
  ([f opts]
   (with-open [stream (util/compressor-input-stream f)]
     (reader/sequential-read-string stream (* 1024 1024 10) 536870912 opts))))

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
    ([this opts]
     (sequential-read (.f this) opts)))
  (read-sequence
    ([this region]
     (protocols/read-sequence this region {}))
    ([this region opts]
     (read-sequence this region opts))))

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
  (let [fasta-exts #"(?i)(\.(fa|fasta|fas|fsa|seq|fna|faa|ffn|frn|mpfa)?)$"]
    (if-let [fai-path (->> ["$1.fai" ".fai" "$1.FAI" ".FAI"]
                           (eduction
                            (comp
                             (map #(cstr/replace fasta-path fasta-exts %))
                             (filter #(.isFile (cio/file %)))))
                           first)]
      (fai/reader fai-path)
      (throw (FileNotFoundException.
              (str "Could not find FASTA Index file for " fasta-path))))))

(defn ^FASTAReader reader
  [f]
  (let [f (.getAbsolutePath (cio/file f))]
    (FASTAReader. (RandomAccessFile. f "r")
                  (util/compressor-input-stream f)
                  (util/as-url f)
                  (delay (fasta-index f)))))

(defn ^FASTAReader clone-reader
  "Clones fasta reader sharing persistent objects."
  [^FASTAReader rdr]
  (let [url (.url rdr)
        raf (RandomAccessFile. (cio/as-file url) "r")
        stream (util/compressor-input-stream url)]
    (FASTAReader. raf stream url (.index-delay rdr))))

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
  (reader/read-sequence rdr chr start end opts))

(defn read
  [rdr]
  (reader/read rdr))

(defn reset
  [rdr]
  (reader/reset rdr))

(defn sequential-read
  ([rdr]
   (sequential-read rdr {}))
  ([^FASTAReader rdr opts]
   (reader/sequential-read-string (.stream rdr) (* 1024 1024 10) 536870912 opts)))

(extend-type FASTAReader
  protocols/IReader
  (reader-url [this] (.url this))
  (read
    ([this] (protocols/read this {}))
    ([this option] (protocols/read-all-sequences this option)))
  (indexed? [this]
    (try
      @(.index-delay this)
      true
      (catch FileNotFoundException _
        false)))
  protocols/IRegionReader
  (read-in-region
    ([this region]
     (protocols/read-in-region this region {}))
    ([this region option]
     (protocols/read-sequence this region option)))
  protocols/ISequenceReader
  (read-indices
    [this] (read-indices this))
  (read-all-sequences
    ([this] (protocols/read-all-sequences this {}))
    ([this opts]
     (sequential-read this opts)))
  (read-sequence
    ([this region]
     (protocols/read-sequence this region {}))
    ([this region opts]
     (read-sequence this region opts))))

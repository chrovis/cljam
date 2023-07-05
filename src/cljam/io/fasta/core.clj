(ns cljam.io.fasta.core
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util]
            [cljam.io.fasta-index.core :as fai]
            [cljam.io.fasta.reader :as reader]
            [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.bgzf.gzi :as gzi])
  (:import [java.io FileNotFoundException RandomAccessFile]
           [cljam.io.fasta.reader FASTAReader IndexedBGZFInputStream]))

;; Reading
;; -------

(defn- fasta-index
  [fasta-url]
  (let [fasta-exts #"(?i)(\.(fa|fasta|fas|fsa|seq|fna|faa|ffn|frn|mpfa))$"]
    (or (->> ["$1.fai" ".fai" "$1.FAI" ".FAI"]
             (map #(util/as-url (cstr/replace (str fasta-url) fasta-exts %)))
             (cons (util/as-url (str fasta-url ".fai")))
             (some #(try (fai/reader %) (catch FileNotFoundException _))))
        (throw (FileNotFoundException.
                (str "Could not find FASTA Index file for " fasta-url))))))

(defn- bgzip-index
  [fasta-url]
  (gzi/read-gzi (util/as-url (str fasta-url ".gzi"))))

(defn- random-accessor [f]
  (if (bgzf/bgzip? f)
    (reader/->IndexedBGZFInputStream
     (bgzf/bgzf-input-stream f)
     (delay (bgzip-index f)))
    (RandomAccessFile. (cio/as-file f) "r")))

(defn reader ^FASTAReader
  [f]
  (let [url (util/as-url f)]
    (FASTAReader. (random-accessor url)
                  (util/compressor-input-stream url)
                  url
                  (delay (fasta-index url)))))

(defn clone-reader
  "Clones fasta reader sharing persistent objects."
  ^FASTAReader
  [^FASTAReader rdr]
  (let [url (.url rdr)
        r (if (instance? RandomAccessFile (.reader rdr))
            (RandomAccessFile. (cio/as-file url) "r")
            (reader/->IndexedBGZFInputStream
             (bgzf/bgzf-input-stream url)
             (.idx ^IndexedBGZFInputStream (.reader rdr))))
        stream (util/compressor-input-stream url)]
    (FASTAReader. r stream url (.index-delay rdr))))

(defn read-headers
  [^FASTAReader rdr]
  (try
    (fai/get-headers @(.index-delay rdr))
    (catch FileNotFoundException _
      (reader/load-headers (.reader rdr)))))

(defn read-seq-summaries
  "Read summaries of sequences in this FASTA file."
  [^FASTAReader rdr]
  (mapv #(select-keys % [:name :len])
        (fai/get-indices @(.index-delay rdr))))

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
  (read-seq-summaries
    [this] (read-seq-summaries this))
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

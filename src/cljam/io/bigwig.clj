(ns cljam.io.bigwig
  "Functions to read the bigWig format. See https://genome.ucsc.edu/goldenpath/help/bigWig.html
  and https://github.com/ucscGenomeBrowser/kent for the detail bigWig
  specifications."
  (:require [clojure.java.io :as cio]
            [cljam.io.protocols :as protocols]
            [cljam.io.util.lsb :as lsb]
            [cljam.util :as util])
  (:import [java.net URL]
           [java.io Closeable IOException RandomAccessFile]))

(def ^:private bigwig-magic 0x888ffc26)

(def ^:private bpt-magic 0x78CA8C91)

(declare read-structure)

(defrecord FixedWidthHeader [magic version zoom-levels chromosome-tree-offset
                             full-data-offset full-index-offset
                             total-summary-offset uncompress-buf-size
                             extension-offset])

(defrecord ZoomHeader [reduction-level data-offset index-offset])

(defrecord TotalSummary [bases-covered min-val max-val sum-data sum-squared])

(defrecord ExtendedHeader [extension-size extra-index-count
                           extra-index-list-offset])

(defrecord BptHeader [block-size key-size val-size item-count root-offset])

; Currently, max number of zoom levels (i.e. max number of zoom headers) is 10,
; the number is small. Therefore, we choose a record containing vector of zoom
; header instead of a flat record.
; Cf. https://github.com/ucscGenomeBrowser/kent/blob/3a0198acd1f859a603f5aad90188bee2d82efe0c/src/inc/bbiFile.h#L384
(defrecord BigWigStructure [^FixedWidthHeader fixed-width-header
                            zoom-headers
                            ^TotalSummary total-summary
                            ^ExtendedHeader extended-header
                            ^BptHeader bpt-header])

(defrecord Chrom [name id size])

(defrecord BIGWIGReader [^RandomAccessFile reader ^URL url]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  protocols/IReader
  (reader-url [this] (.url this))
  (read [this] (read-structure this))
  (read [this option] (read-structure this))
  (indexed? [_] false))

(defn ^BIGWIGReader reader
  "Returns an open cljam.io.bigwig.BIGWIGReader of f. Should be used inside with-open
  to ensure the reader is properly closed."
  [f]
  (let [f (.getAbsolutePath (cio/file f))
        reader (RandomAccessFile. f "r")]
    (BIGWIGReader. reader (util/as-url f))))

(defn- check-bigwig-magic
  "Checks if the magic is right for bigWig format. Otherwise, throws IOException."
  [uint]
  (when-not (= uint bigwig-magic)
    (throw (IOException. "Invalid bigWig magic"))))

(defn- check-version
  "Ranged from [1,4]. Throws IOException if the version is out of range."
  [ushort]
  (when-not (and (<= 1 ushort) (<= ushort 4))
    (throw (IOException. "Invalid bigWig version"))))

(defn- check-field-count
  "For bigWig 0. Throws IOException if the fieldCount is invalid."
  [ushort]
  (when-not (zero? ushort)
    (throw (IOException. "Invalid bigWig fieldCount"))))

(defn- check-defined-field-count
  "For bigWig 0. Throws IOException if the definedFieldCount is invalid."
  [ushort]
  (when-not (zero? ushort)
    (throw (IOException. "Invalid bigWig definedFieldCount"))))

(defn- check-auto-sql-offset
  "For bigWig 0. Throws IOException if the autoSqlOffset is invalid."
  [^long long]
  (when-not (zero? long)
    (throw (IOException. "Invalid bigWig autoSqlOffset"))))

(defn- read-fixed-width-header
  "Returns a FixedWidthHeader."
  [^RandomAccessFile r]
  (let [magic (lsb/read-uint r)
        version (lsb/read-ushort r)
        zoom-levels (lsb/read-ushort r)
        chromosome-tree-offset (lsb/read-long r)
        full-data-offset (lsb/read-long r)
        full-index-offset (lsb/read-long r)
        field-count (lsb/read-ushort r)
        defined-field-count (lsb/read-ushort r)
        auto-sql-offset (lsb/read-long r)
        total-summary-offset (lsb/read-long r)
        uncompress-buf-size (lsb/read-uint r)
        extension-offset (lsb/read-long r)]
    (check-bigwig-magic magic)
    (check-version version)
    (check-field-count field-count)
    (check-defined-field-count defined-field-count)
    (check-auto-sql-offset auto-sql-offset)
    (FixedWidthHeader.
     magic version zoom-levels chromosome-tree-offset full-data-offset
     full-index-offset total-summary-offset uncompress-buf-size
     extension-offset)))

(defn- read-zoom-headers
  "Returns a vector of ZoomHeader from reader."
  [^RandomAccessFile r {:keys [zoom-levels]}]
  (letfn [(read-zoom-header [n acc]
            (if (zero? n)
              acc
              (let [reduction-level (lsb/read-uint r)
                    _reserved (lsb/read-uint r)
                    data-offset (lsb/read-long r)
                    index-offset (lsb/read-long r)]
                (recur (dec n)
                       (conj acc
                             (ZoomHeader.
                              reduction-level data-offset index-offset))))))]
    (read-zoom-header zoom-levels [])))

(defn- read-total-summary
  "Returns a totalSummay. If it isn't present, returns nil."
  [^RandomAccessFile r {:keys [total-summary-offset]}]
  (when-not (zero? total-summary-offset)
    (.seek r total-summary-offset)
    (let [bases-covered (lsb/read-long r)
          min-val (lsb/read-double r)
          max-val (lsb/read-double r)
          sum-data (lsb/read-double r)
          sum-squared (lsb/read-double r)]
      (TotalSummary.
       bases-covered min-val max-val sum-data sum-squared))))

(defn- read-extended-header
  "Returns an extendedHeader. It it isn't present, returns nil."
  [^RandomAccessFile r {:keys [extension-offset]}]
  (when-not (zero? extension-offset)
    (.seek r extension-offset)
    (let [extension-size (lsb/read-ushort r)
          extra-index-count (lsb/read-ushort r)
          extra-index-list-offset (lsb/read-long r)]
      (ExtendedHeader.
       extension-size extra-index-count extra-index-list-offset))))

(defn- check-bpt-magic
  "Checks if the magic is right for bpt format. Otherwise, throws IOException."
  [uint]
  (when-not (= uint bpt-magic)
    (throw (IOException. "Invalid bpt magic"))))

(defn- read-bpt-header
  "Returns B+ tree data."
  [^RandomAccessFile r {:keys [chromosome-tree-offset]}]
  (.seek r chromosome-tree-offset)
  (let [magic (lsb/read-uint r)
        block-size (lsb/read-uint r)
        key-size (lsb/read-uint r)
        val-size (lsb/read-uint r)
        item-count (lsb/read-long r)]
    (check-bpt-magic magic)
    (lsb/skip r 8)
    (let [root-offset (.getFilePointer r)]
      (BptHeader.
       block-size key-size val-size item-count root-offset))))

(defn- read-all-headers
  "Returns the all headers of bigWig format."
  [^RandomAccessFile r]
  (let [fixed-width-header (read-fixed-width-header r)
        zoom-headers (read-zoom-headers r fixed-width-header)
        total-summary (read-total-summary r fixed-width-header)
        extended-header (read-extended-header r fixed-width-header)
        bpt-header (read-bpt-header r fixed-width-header)]
    (BigWigStructure.
     fixed-width-header zoom-headers total-summary extended-header bpt-header)))

(defn- read-leafs
  "Returns the Chrom data of leafs."
  [^RandomAccessFile r key-size child-count]
  (repeatedly child-count
              (fn [_]
                (let [name (->> (lsb/read-bytes r key-size)
                                (map char)
                                (apply str))
                      id (lsb/read-uint r)
                      size (lsb/read-uint r)]
                  (Chrom.
                   name id size)))))

(defn- read-file-offsets
  "Skips offsets and returns the file offsets of children."
  [^RandomAccessFile r key-size child-count]
  (repeatedly child-count
              (fn [_]
                (lsb/skip r key-size)
                (lsb/read-long r))))

(defn- read-chroms
  "Returns a sequence of Chrom data."
  [^RandomAccessFile r {:keys [key-size val-size root-offset]}]
  (letfn [(traverse [block-start]
            (.seek r block-start)
            (let [leaf? (-> r lsb/read-ubyte zero? not)
                  _reversed (lsb/read-ubyte r)
                  child-count (lsb/read-ushort r)]
              (if leaf?
                (read-leafs r key-size child-count)
                (let [file-offsets (read-file-offsets r key-size child-count)]
                  (map traverse file-offsets)))))]
    (traverse root-offset)))

(defn read-structure
  "Reads a bigWig tracks from reader and returns a map with two values,
  a header structure and a sequence of chromosome data."
  [^BIGWIGReader rdr]
  (let [r ^RandomAccessFile (.reader rdr)]
    (.seek r 0)
    (let [headers (read-all-headers r)
          chroms (read-chroms r (:bpt-header headers))]
      {:headers headers, :chroms chroms})))

(ns cljam.io.crai
  (:require [cljam.util :as util]
            [cljam.util.intervals :as intervals]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io Closeable OutputStreamWriter PrintWriter]))

(defn read-index
  "Reads a CRAI file `f` and creates an index."
  [f refs]
  (let [refs (vec refs)]
    (with-open [rdr (io/reader (util/compressor-input-stream f))]
      (->> (line-seq rdr)
           (map (fn [line]
                  (let [[^long seq-id ^long start ^long span container-offset slice-offset size]
                        (map #(Long/parseLong %) (str/split line #"\t"))
                        unmapped? (neg? seq-id)]
                    {:chr (if unmapped? "*" (:name (nth refs seq-id)))
                     :start (if unmapped? 0 start)
                     :end (if unmapped? 0 (+ start span))
                     :container-offset container-offset
                     :slice-offset slice-offset
                     :size size})))
           intervals/index-intervals))))

(defn find-overlapping-entries
  "Finds and returns all entries from the index that overlap with the specified
  region."
  [idx chr start end]
  (intervals/find-overlap-intervals idx chr start end))

(deftype CRAIWriter [^PrintWriter writer]
  Closeable
  (close [_]
    (.close writer)))

(defn writer
  "Creates and returns a CRAI writer for the file `f`."
  ^Closeable [f]
  (-> (util/compressor-output-stream f :gzip)
      OutputStreamWriter.
      PrintWriter.
      ->CRAIWriter))

(defn- write-index-entry
  [^CRAIWriter wtr {:keys [^long ref-seq-id start span container-offset slice-offset size]}]
  (let [unmapped? (neg? ref-seq-id)]
    (doto ^PrintWriter (.-writer wtr)
      (.print ref-seq-id)
      (.print \tab)
      (.print (if unmapped? 0 (long start)))
      (.print \tab)
      (.print (if unmapped? 0 (long span)))
      (.print \tab)
      (.print (long container-offset))
      (.print \tab)
      (.print (long slice-offset))
      (.print \tab)
      (.print (long size))
      (.print \newline))))

(defn write-index-entries
  "Writes CRAM index entries.

  Each CRAM index entry consists of the following keys:
    - ref-seq-id: Reference sequence id (-1 for slices containing only unmapped records)
    - start: Alignment start
    - span: Alignment span
    - container-offset: Absolute byte offset of the container header in the file
    - slice-offset: Relative byte offset of the slice header block
    - size: Slice size in bytes

  Note that if :ref-seq-id equals to -1 (unmapped), the CRAI writer will ignore
  the actual values of :start/:span and write zeros for both fields."
  [^CRAIWriter wtr index-entries]
  (run! (partial write-index-entry wtr) index-entries))

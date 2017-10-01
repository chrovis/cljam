(ns cljam.io.fastq
  "Functions to read and write the FASTQ format."
  (:require [clojure.java.io :as cio]
            [clojure.string :as string]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util])
  (:import [java.io Closeable]))

(declare read-sequences write-sequences)

(deftype FASTQReader [reader f]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this)))
  protocols/IReader
  (reader-path [this] (.f this))
  (read [this] (read-sequences this))
  (read [this opts] (read-sequences this opts))
  (indexed? [_] false))

(deftype FASTQWriter [writer f]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this)))
  protocols/IWriter
  (writer-path [this] (.f this)))

(defn ^FASTQReader reader
  "Returns an open cljam.io.fastq.FASTQReader of f. Should be used inside
  with-open to ensure the reader is properly closed."
  [^String f]
  (let [file (cio/file f)
        path (.getAbsolutePath file)]
    (-> (util/compressor-input-stream path)
        cio/reader
        (FASTQReader. path))))

(defn ^FASTQWriter writer
  "Returns an open cljam.io.fastq.FASTQWriter of f. Should be used inside
  with-open to ensure the writer is properly closed."
  [^String f]
  (let [file (cio/file f)
        path (.getAbsolutePath file)]
    (-> (util/compressor-output-stream path)
        cio/writer
        (FASTQWriter. path))))

(defrecord FASTQRead [^String name ^String sequence quality])

(defn- ^FASTQRead deserialize-fastq
  "Deserialize a read from 4 lines of fastq file."
  [[^String name-line ^String seq-line ^String plus-line ^String qual-line]
   {:keys [decode-quality] :or {decode-quality :phred33}}]
  {:pre [(not-empty name-line)
         (not-empty seq-line)
         (not-empty plus-line)
         (not-empty qual-line)
         (= (first name-line) \@)
         (= (first plus-line) \+)
         (not-empty (rest name-line))
         (or (empty? (rest plus-line))
             (= (rest plus-line) (rest name-line)))
         (= (count seq-line) (count qual-line))]
   :post [(every? (fn [q] (case decode-quality
                            :phred33 (<= 0 q 93)
                            :phred64 (<= 0 q 62)
                            true))
                  (:quality %))]}
  (FASTQRead.
   (subs name-line 1)
   seq-line
   (case decode-quality
     :phred33 (map #(- (int %) 33) qual-line)
     :phred64 (map #(- (int %) 64) qual-line)
     qual-line)))

(defn read-sequences
  "Returns a lazy sequence of FASTQReads deserialized from given reader."
  ([rdr]
   (read-sequences rdr {}))
  ([^FASTQReader rdr opts]
   (sequence
    (comp (map string/trim)
          (partition-all 4)
          (map #(deserialize-fastq % opts)))
    (line-seq (.reader rdr)))))

(defn- ^String serialize-fastq
  "Serialize a FASTQRead to FASTQ format string."
  [^FASTQRead {:keys [name sequence quality]}
   {:keys [encode-quality] :or {encode-quality :phred33}}]
  {:pre [(not-empty name)
         (not-empty sequence)
         (not-empty quality)
         (= (count sequence) (count quality))
         (every? #(case encode-quality
                    :phred33 (<= 0 % 93)
                    :phred64 (<= 0 % 62)
                    true) quality)]}
  (-> [(str "@" name)
       sequence
       "+"
       (apply str (map #(case encode-quality
                          :phred33 (char (+ % 33))
                          :phred64 (char (+ % 64))
                          %)
                       quality))]
      (interleave (repeat \newline))
      (as-> x (apply str x))))

(defn write-sequences
  "Write given sequence of reads to a FASTQ file."
  ([wtr sequences]
   (write-sequences wtr sequences {}))
  ([^FASTQWriter wtr sequences opts]
   (let [w ^java.io.Writer (.writer wtr)]
     (doseq [s sequences]
       (.write w ^String (serialize-fastq s opts))))))

(def casava-pattern #"^@?([^\s^:]+):(\d+):(\d+):(\d+):(\d+)#(\d+)/(\d)+$")
(defn deserialize-casava-name
  "Parse Casava-style name of fastq read."
  [^String name]
  (let [[match instrument lane tile x y index pair]
        (re-matches casava-pattern name)]
    (when match
      {:instrument instrument
       :lane (Integer/parseInt lane)
       :tile (Integer/parseInt tile)
       :x (Integer/parseInt x)
       :y (Integer/parseInt y)
       :index (Integer/parseInt index)
       :pair (Integer/parseInt pair)})))

(defn ^String serialize-casava-name
  "Encode fastq name map to Casava-style string."
  [{:keys [instrument lane tile x y index pair]}]
  (when (and instrument lane tile x y index pair)
    (str instrument \: lane \: tile \: x \: y \# index \/ pair)))

(def casava-1_8-pattern
  #"^@?([^\s^:]+):(\d+):([^\s^\:]+):(\d+):(\d+):(\d+):(\d+)\s+(\d+):(Y|N):(\d+):(\S+)$")
(defn deserialize-casava-1_8-name
  "Parse Casava1.8-style name of fastq read."
  [^String name]
  (let [[match instrument run flowcell lane tile x y pair filtered control index]
        (re-matches casava-1_8-pattern name)]
    (when match
      {:instrument instrument
       :run (Integer/parseInt run)
       :flowcell flowcell
       :lane (Integer/parseInt lane)
       :tile (Integer/parseInt tile)
       :x (Integer/parseInt x)
       :y (Integer/parseInt y)
       :pair (Integer/parseInt pair)
       :filtered (= filtered "Y")
       :control (Integer/parseInt control)
       :index index})))

(defn ^String serialize-casava-1_8-name
  "Encode fastq name map to Casava1.8-style string."
  [{:keys [instrument run flowcell lane tile x y pair filtered control index]}]
  (when (and instrument run flowcell lane tile x y pair (not (nil? filtered)) control index)
    (str instrument \: run \: flowcell \: lane \: tile \: x \: y " "
         pair \: (if filtered \Y \N) \: control \: index)))

(defn deserialize-name
  "Try parsing name of fastq read."
  [^String name]
  (first (keep #(% name) [deserialize-casava-1_8-name deserialize-casava-name])))

(defn ^String serialize-name
  "Try encoding name of fastq read."
  [name]
  (first (keep #(% name) [serialize-casava-1_8-name serialize-casava-name])))

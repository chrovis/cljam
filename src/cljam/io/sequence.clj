(ns cljam.io.sequence
  "Functions to read and write formats representing sequences such as FASTA and
  TwoBit."
  (:refer-clojure :exclude [indexed?])
  (:require [cljam.io.fasta.core :as fa-core]
            [cljam.io.fasta.writer :as fa-writer]
            [cljam.io.protocols :as protocols]
            [cljam.io.twobit.reader :as tb-reader]
            [cljam.io.twobit.writer :as tb-writer]
            [cljam.io.util :as io-util])
  (:import java.io.Closeable
           cljam.io.fasta.reader.FASTAReader
           cljam.io.fasta.writer.FASTAWriter
           cljam.io.twobit.reader.TwoBitReader
           cljam.io.twobit.writer.TwoBitWriter))

;; Reading
;; -------

(defn fasta-reader
  "Returns an open cljam.io.fasta.reader.FASTAReader of f. Should be used inside
  with-open to ensure the reader is properly closed."
  ^FASTAReader
  [f]
  (fa-core/reader f))

(defn twobit-reader
  "Returns an open cljam.io.twobit.reader.TwoBitReader of f. Should be used
  inside with-open to ensure the reader is properly closed."
  ^TwoBitReader
  [f]
  (tb-reader/reader f))

(defn reader
  "Selects suitable reader from f's extension, returning the open reader. Opens
  a new reader if the arg represents a file such as String path, java.io.File,
  or java.net.URL. If a reader is given, clones the reader. This function
  supports FASTA and TwoBit formats."
  ^Closeable
  [f]
  (cond
    (io-util/fasta-reader? f) (fa-core/clone-reader f)
    (io-util/twobit-reader? f) (tb-reader/clone-reader f)
    :else (case (try
                  (io-util/file-type f)
                  (catch IllegalArgumentException _
                    (io-util/file-type-from-contents f)))
            :fasta (fasta-reader f)
            :2bit (twobit-reader f)
            (throw (IllegalArgumentException. "Invalid file type")))))

(defn read-sequence
  "Reads sequence in region of FASTA/TwoBit file."
  ([rdr region] (protocols/read-sequence rdr region))
  ([rdr region option] (protocols/read-sequence rdr region option)))

(defn read-all-sequences
  "Reads all sequences of FASTA/TwoBit file."
  ([rdr] (protocols/read-all-sequences rdr))
  ([rdr option] (protocols/read-all-sequences rdr option)))

(defn read-seq-summaries
  "Returns summaries of sequences in FASTA/TwoBit file. Returns a vector of maps
  containing `:name` and `:len`."
  [rdr]
  (protocols/read-seq-summaries rdr))

(defn read-indices
  "Reads metadata of indexed sequences. Returns a vector of maps containing
  `:name`, `:len` and other format-specific keys. Forces loading all indices."
  [rdr]
  (protocols/read-indices rdr))

(defn indexed?
  "Returns true if the reader can be randomly accessed, false if not. Note this
  function immediately realizes a delayed index."
  [rdr]
  (protocols/indexed? rdr))

;; Writing
;; -------

(defn fasta-writer
  "Returns an open cljam.io.fasta.writer.FASTAWriter of f with options:
    :cols - Maximum number of characters written in one row.
    :create-index? - If true, .fai will be created simultaneously.
  Should be used inside with-open to ensure the writer is properly closed."
  (^FASTAWriter [f]
   (fasta-writer f {}))
  (^FASTAWriter [f options]
   (fa-writer/writer f options)))

(defn twobit-writer
  "Returns an open cljam.io.twobit.writer.TwoBitWriter of f with options:
    :index - metadata of indexed sequences. The amount of memory usage can be
      reduced if index is supplied.
  Should be used inside with-open to ensure the writer is properly closed."
  (^TwoBitWriter [f]
   (twobit-writer f {}))
  (^TwoBitWriter [f options]
   (tb-writer/writer f options)))

(defn writer
  "Selects suitable writer from f's extension, returning the open writer. This
  function supports FASTA and TwoBit format."
  ^Closeable
  [f & options]
  (case (io-util/file-type f)
    :fasta (apply fasta-writer f options)
    :2bit (apply twobit-writer f options)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn write-sequences
  "Writes all sequences to FASTA/TwoBit file."
  [wtr seqs]
  (protocols/write-sequences wtr seqs))

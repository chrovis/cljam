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

(defn ^FASTAReader fasta-reader
  "Returns an open cljam.io.fasta.reader.FASTAReader of f. Should be used inside
  with-open to ensure the reader is properly closed."
  [f]
  (fa-core/reader f))

(defn ^TwoBitReader twobit-reader
  "Returns an open cljam.io.twobit.reader.TwoBitReader of f. Should be used
  inside with-open to ensure the reader is properly closed."
  [f]
  (tb-reader/reader f))

(defn ^Closeable reader
  "Selects suitable reader from f's extension, returning the open reader. Opens
  a new reader if given a path, clones the reader if given a reader. This
  function supports FASTA and TwoBit formats."
  [f]
  (if (string? f)
    (case (io-util/file-type f)
      :fasta (fasta-reader f)
      :2bit (twobit-reader f)
      (throw (IllegalArgumentException. "Invalid file type")))
    (cond
      (io-util/fasta-reader? f) (fa-core/clone-reader f)
      (io-util/twobit-reader? f) (tb-reader/clone-reader f)
      :else (throw (IllegalArgumentException. "Invalid reader type")))))

(defn read-sequence
  "Reads sequence in region of FASTA/TwoBit file."
  ([rdr region] (protocols/read-sequence rdr region))
  ([rdr region option] (protocols/read-sequence rdr region option)))

(defn read-all-sequences
  "Reads all sequences of FASTA/TwoBit file."
  ([rdr] (protocols/read-all-sequences rdr))
  ([rdr option] (protocols/read-all-sequences rdr option)))

(defn read-indices
  "Reads metadata of indexed sequences. Returns a vector of maps containing
  :name, :len and other format-specific keys."
  [rdr]
  (protocols/read-indices rdr))

(defn indexed?
  "Returns true if the reader can be randomly accessed, false if not. Note this
  function immediately realizes a delayed index."
  [rdr]
  (protocols/indexed? rdr))

;; Writing
;; -------

(defn ^FASTAWriter fasta-writer
  "Returns an open cljam.io.fasta.writer.FASTAWriter of f with options:
    :cols - Maximum number of characters written in one row.
    :create-index? - If true, .fai will be created simultaneously.
  Should be used inside with-open to ensure the writer is properly closed."
  ([f]
   (fasta-writer f {}))
  ([f options]
   (fa-writer/writer f options)))

(defn ^TwoBitWriter twobit-writer
  "Returns an open cljam.io.twobit.writer.TwoBitWriter of f with options:
    :index - metadata of indexed sequences. The amount of memory usage can be
      reduced if index is supplied.
  Should be used inside with-open to ensure the writer is properly closed."
  ([f]
   (twobit-writer f {}))
  ([f options]
   (tb-writer/writer f options)))

(defn ^Closeable writer
  "Selects suitable writer from f's extension, returning the open writer. This
  function supports FASTA and TwoBit format."
  [f & options]
  (case (io-util/file-type f)
    :fasta (apply fasta-writer f options)
    :2bit (apply twobit-writer f options)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn write-sequences
  "Writes all sequences to FASTA/TwoBit file."
  [wtr seqs]
  (protocols/write-sequences wtr seqs))

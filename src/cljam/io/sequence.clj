(ns cljam.io.sequence
  "Functions to read and write formats representing sequences such as FASTA and
  TwoBit."
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
  "Selects suitable reader from f's extension, returning the open reader. This
  function supports FASTA and TwoBit formats."
  [f]
  (case (io-util/file-type f)
    :fasta (fasta-reader f)
    :2bit (twobit-reader f)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn read-sequence
  "Reads sequence in region of FASTA/TwoBit file."
  ([rdr region] (protocols/read-sequence rdr region))
  ([rdr region option] (protocols/read-sequence rdr region option)))

(defn read-all-sequences
  "Reads all sequences of FASTA/TwoBit file."
  ([rdr] (protocols/read-all-sequences rdr))
  ([rdr option] (protocols/read-all-sequences rdr option)))

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
  "Returns an open cljam.io.twobit.writer.TwoBitWriter of f. Should be used
  inside with-open to ensure the writer is properly closed."
  [f]
  (tb-writer/writer f))

(defn ^Closeable writer
  "Selects suitable writer from f's extension, returning the open writer. This
  function supports FASTA and TwoBit format."
  [f & options]
  (case (io-util/file-type f)
    :fasta (apply fasta-writer f options)
    :2bit (twobit-writer f)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn write-sequences
  "Writes all sequences to FASTA/TwoBit file."
  [wtr seqs]
  (protocols/write-sequences wtr seqs))

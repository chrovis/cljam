(ns cljam.io.sam
  "Functions to read and write the SAM (Sequence Alignment/Map) format and BAM
  (its binary equivalent). See https://samtools.github.io/hts-specs/ for the
  detail SAM/BAM specifications."
  (:refer-clojure :exclude [indexed?])
  (:require [cljam.io.sam.reader :as sam-reader]
            [cljam.io.sam.writer :as sam-writer]
            [cljam.io.bam.core :as bam-core]
            [cljam.io.protocols :as protocols]
            [cljam.io.util :as io-util])
  (:import java.io.Closeable
           cljam.io.sam.reader.SAMReader
           cljam.io.sam.writer.SAMWriter
           cljam.io.bam.reader.BAMReader
           cljam.io.bam.writer.BAMWriter))

;; Reading
;; -------

(defn ^SAMReader sam-reader
  "Returns an open cljam.io.sam.reader.SAMReader of f. Should be used inside
  with-open to ensure the reader is properly closed."
  [f]
  (sam-reader/reader f))

(defn ^BAMReader bam-reader
  "Returns an open cljam.io.bam.reader.BAMReader of f. Should be used inside
  with-open to ensure the reader is properly closed."
  [f]
  (bam-core/reader f))

(defn ^BAMReader clone-bam-reader
  "Clones bam reader sharing persistent objects."
  [r]
  (bam-core/clone-reader r))

(defn ^Closeable reader
  "Selects suitable reader from f's extension, returning the reader. Opens a new
  reader if the arg represents a file such as String path, java.io.File, or
  java.net.URL. If a reader is given, clones the reader. This function supports
  SAM and BAM formats."
  [f]
  (if (io-util/bam-reader? f)
    (clone-bam-reader f)
    (case (io-util/file-type f)
      :sam (sam-reader f)
      :bam (bam-reader f)
      (throw (IllegalArgumentException. "Invalid source type")))))

(defn read-header
  "Returns header of the SAM/BAM file."
  [rdr]
  (protocols/read-header rdr))

(defn read-refs
  "Returns references of the SAM/BAM file."
  [rdr]
  (protocols/read-refs rdr))

(defn read-alignments
  "Reads alignments of the SAM/BAM file, returning the alignments as an eduction."
  ([rdr] (protocols/read-alignments rdr))
  ([rdr region] (protocols/read-alignments rdr region)))

(defn read-blocks
  "Reads alignment blocks of the SAM/BAM file, returning the blocks as an eduction."
  ([rdr] (protocols/read-blocks rdr))
  ([rdr region] (protocols/read-blocks rdr region))
  ([rdr region option] (protocols/read-blocks rdr region option)))

(defn indexed?
  "Returns true if the reader can be randomly accessed, false if not. Note this
  function immediately realizes a delayed index."
  [rdr]
  (protocols/indexed? rdr))

;; Writing
;; -------

(defn ^SAMWriter sam-writer
  "Returns an open cljam.io.sam.writer.SAMWriter of f. Should be used inside
  with-open to ensure the writer is properly closed."
  [f]
  (sam-writer/writer f))

(defn ^BAMWriter bam-writer
  "Returns an open cljam.io.bam.writer.BAMWriter of f. Should be used inside
  with-open to ensure the writer is properly closed."
  [f]
  (bam-core/writer f))

(defn ^Closeable writer
  "Selects suitable writer from f's extension, returning the writer. This
  function supports SAM and BAM format."
  [f]
  (case (io-util/file-type f)
    :sam (sam-writer f)
    :bam (bam-writer f)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn write-header
  "Writes header to the SAM/BAM file."
  [wtr header]
  (protocols/write-header wtr header))

(defn write-refs
  "Writes references to the SAM/BAM file."
  [wtr header]
  (protocols/write-refs wtr header))

(defn write-alignments
  "Writes alignments to the SAM/BAM file."
  [wtr alignments header]
  (protocols/write-alignments wtr alignments header))

(defn write-blocks
  "Writes alignment blocks of the SAM/BAM file."
  [wtr blocks]
  (protocols/write-blocks wtr blocks))

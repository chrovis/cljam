(ns cljam.io.sam
  "Functions to read and write the SAM (Sequence Alignment/Map) format and BAM
  (its binary equivalent). See https://samtools.github.io/hts-specs/ for the
  detail SAM/BAM specifications."
  (:refer-clojure :exclude [indexed?])
  (:require [cljam.io.sam.reader :as sam-reader]
            [cljam.io.sam.writer :as sam-writer]
            [cljam.io.bam.core :as bam-core]
            [cljam.io.protocols :as protocols]
            [cljam.io.util :as io-util]
            [cljam.io.sam.util.flag :as flag])
  (:import java.io.Closeable
           cljam.io.sam.reader.SAMReader
           cljam.io.sam.writer.SAMWriter
           cljam.io.bam.reader.BAMReader
           cljam.io.bam.writer.BAMWriter))

;; Reading
;; -------

(defn sam-reader
  "Returns an open cljam.io.sam.reader.SAMReader of f. Should be used inside
  with-open to ensure the reader is properly closed."
  ^SAMReader
  [f]
  (sam-reader/reader f))

(defn bam-reader
  "Returns an open cljam.io.bam.reader.BAMReader of f. Should be used inside
  with-open to ensure the reader is properly closed."
  ^BAMReader
  [f]
  (bam-core/reader f))

(defn clone-bam-reader
  "Clones bam reader sharing persistent objects."
  ^BAMReader
  [r]
  (bam-core/clone-reader r))

(defn reader
  "Selects suitable reader from f's extension, returning the reader. Opens a new
  reader if the arg represents a file such as String path, java.io.File, or
  java.net.URL. If a reader is given, clones the reader. This function supports
  SAM and BAM formats."
  ^Closeable
  [f]
  (if (io-util/bam-reader? f)
    (clone-bam-reader f)
    (case (try
            (io-util/file-type f)
            (catch IllegalArgumentException _
              (io-util/file-type-from-contents f)))
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

(defn read-mate-alignments
  "Reads the mate alignments corresponding to the given `alignments` from the
  `rdr`. A mate alignment here refers to a primary alignment that has the same
  QNAME as the given alignment and is the opposite R1/R2 pair. The `rdr` must
  be indexed. Returns an eduction."
  ([rdr alignments]
   (protocols/read-mate-alignments rdr alignments))
  ([rdr options alignments]
   (protocols/read-mate-alignments rdr options alignments)))

(defn make-pairs
  "Pairs mate alignments from the given `alignments` and `rdr`.

  This is a convenient wrapper for `read-mate-alignments`. This function
  processes the alignments provided in the `alignments` sequence, pairing mate
  alignments based on their QNAMEs. If a mate alignment is missing from
  `alignments`, it attempts to read the mate alignment from the reader `rdr`.
  Non-primary alignments are ignored. The order of the paired alignments in the
  output is not guaranteed."
  ([rdr alignments]
   (make-pairs rdr {} alignments))
  ([rdr options alignments]
   (letfn [(has-mate? [{:keys [flag]}]
             (and (flag/multiple? flag)
                  (flag/primary? flag)))]
     (let [{solos true,
            pairs false} (->> alignments
                              (group-by :qname)
                              (keep
                               (fn [[_ vs]] (not-empty (filterv has-mate? vs))))
                              (group-by #(= 1 (count %))))
           solos (mapv first solos)
           mates (->> solos
                      (read-mate-alignments rdr options)
                      (into {} (map (juxt :qname identity))))]
       (concat pairs
               (sequence
                (comp (map (juxt identity (comp mates :qname)))
                      (filter second)) solos))))))

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

(defn sam-writer
  "Returns an open cljam.io.sam.writer.SAMWriter of f. Should be used inside
  with-open to ensure the writer is properly closed."
  ^SAMWriter
  [f]
  (sam-writer/writer f))

(defn bam-writer
  "Returns an open cljam.io.bam.writer.BAMWriter of f. Should be used inside
  with-open to ensure the writer is properly closed."
  (^BAMWriter [f]
   (bam-writer f false))
  (^BAMWriter [f create-index?]
   (bam-core/writer f create-index?)))

(defn writer
  "Selects suitable writer from f's extension, returning the writer. This
  function supports SAM and BAM format."
  (^Closeable [f]
   (writer f false))
  (^Closeable [f create-index?]
   (case (io-util/file-type f)
     :sam (if create-index?
            (throw (ex-info "SAM file indexing is not implemented." {}))
            (sam-writer f))
     :bam (bam-writer f create-index?)
     (throw (IllegalArgumentException. "Invalid file type")))))

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

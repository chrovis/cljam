(ns cljam.io.vcf
  "Functions to read and write the VCF (Variant Call Format) format and BCF (its
  binary equivalent). See https://samtools.github.io/hts-specs/ for the detail
  VCF/BCF specifications."
  (:refer-clojure :exclude [indexed?])
  (:require [clojure.java.io :as cio]
            [cljam.util :as util]
            [cljam.io.protocols :as protocols]
            [cljam.io.util :as io-util]
            [cljam.io.vcf.reader :as vcf-reader]
            [cljam.io.vcf.writer :as vcf-writer]
            [cljam.io.bcf.reader :as bcf-reader]
            [cljam.io.bcf.writer :as bcf-writer]
            [cljam.io.util.bgzf :as bgzf]
            [cljam.io.tabix :as tabix]
            [cljam.io.csi :as csi])
  (:import java.io.Closeable
           java.io.FileNotFoundException
           cljam.io.vcf.reader.VCFReader
           cljam.io.vcf.writer.VCFWriter
           cljam.io.bcf.reader.BCFReader
           cljam.io.bcf.writer.BCFWriter))

;; Reading
;; -------

(defn vcf-reader
  "Returns an open cljam.io.vcf.reader.VCFReader of f. Should be used inside
  with-open to ensure the reader is properly closed."
  ^VCFReader
  [f]
  (let [meta-info (with-open [r (cio/reader (util/compressor-input-stream f))]
                    (vcf-reader/load-meta-info r))
        header (with-open [r (cio/reader (util/compressor-input-stream f))]
                 (vcf-reader/load-header r))]
    (VCFReader. (util/as-url f) meta-info header
                (if (bgzf/bgzip? f)
                  (bgzf/bgzf-input-stream f)
                  (cio/reader (util/compressor-input-stream f)))
                (delay (try (csi/read-index (str f ".csi"))
                            (catch FileNotFoundException _
                              (tabix/read-index (str f ".tbi"))))))))

(defn bcf-reader
  "Returns an open cljam.io.bcf.reader.BCFReader of f. Should be used inside
  with-open to ensure the reader is properly closed. Throws IOException if
  failed to parse BCF file format."
  ^BCFReader
  [f]
  (bcf-reader/reader f))

(defn clone-vcf-reader
  "Clones vcf reader sharing persistent objects."
  ^VCFReader
  [^VCFReader rdr]
  (let [url (.url rdr)
        input-stream (if (bgzf/bgzip? url)
                       (bgzf/bgzf-input-stream url)
                       (cio/reader (util/compressor-input-stream url)))]
    (VCFReader. url (.meta-info rdr) (.header rdr)
                input-stream
                (.index-delay rdr))))

(defn clone-bcf-reader
  "Clones bcf reader sharing persistent objects."
  ^BCFReader
  [^BCFReader rdr]
  (let [url (.url rdr)
        input-stream (bgzf/bgzf-input-stream url)]
    (BCFReader. (.url rdr) (.meta-info rdr) (.header rdr)
                input-stream (.start-pos rdr) (.index-delay rdr))))

(defn clone-reader
  "Clones vcf/bcf reader sharing persistent objects."
  ^Closeable
  [rdr]
  (cond
    (io-util/vcf-reader? rdr) (clone-vcf-reader rdr)
    (io-util/bcf-reader? rdr) (clone-bcf-reader rdr)
    :else (throw (IllegalArgumentException. "Invalid file type"))))

(defn reader
  "Selects suitable reader from f's extension, returning the open reader. This
  function supports VCF and BCF formats."
  ^Closeable
  [f]
  (if (or (io-util/vcf-reader? f)
          (io-util/bcf-reader? f))
    (clone-reader f)
    (case (try (io-util/file-type f)
               (catch IllegalArgumentException _
                 (io-util/file-type-from-contents f)))
      :vcf (vcf-reader f)
      :bcf (bcf-reader f)
      (throw (IllegalArgumentException. "Invalid file type")))))

(defn meta-info
  "Returns meta-info section of VCF/BCF file as a map."
  [rdr]
  (protocols/meta-info rdr))

(defn header
  "Returns header of VCF/BCF file as a sequence of strings."
  [rdr]
  (protocols/header rdr))

(defn indexed?
  "Returns true if the reader can be randomly accessed, false if not. Note this
  function immediately realizes a delayed index."
  [rdr]
  (protocols/indexed? rdr))

(defn read-variants
  "Reads variants of the VCF/BCF file, returning them as a lazy sequence. rdr
  must implement cljam.io.protocols/IVariantReader. Can take an option :depth to
  specify parsing level, default :deep. <:deep|:vcf|:bcf|:shallow|:raw>
    :deep - Fully parsed variant map. FORMAT, FILTER, INFO and samples columns are parsed.
    :vcf - VCF-style map. FORMAT, FILTER, INFO and samples columns are strings.
    :bcf - BCF-style map. CHROM, FILTER, INFO and :genotype contains indices to meta-info.
    :shallow - Only CHROM, POS and ref-length are parsed.
    :raw - Raw map of ByteBufers."
  ([rdr] (protocols/read-variants rdr))
  ([rdr option] (protocols/read-variants rdr option)))

(defn read-variants-randomly
  "Reads variants of the VCF/BCF file randomly, returning them as a lazy sequence."
  ([rdr span-option depth-option]
   (protocols/read-variants-randomly
    rdr
    span-option
    depth-option)))

(defn read-file-offsets
  "Reads offsets {:file-beg :file-end :beg :end :chr } from VCF/BCF file."
  [rdr] (protocols/read-file-offsets rdr))

;; Writing
;; -------

(defn vcf-writer
  "Returns an open cljam.io.vcf.writer.VCFWriter of f. Meta-information lines
  and a header line will be written in this function. Should be used inside
  with-open to ensure the writer is properly closed. e.g.

    (with-open [wtr (vcf-writer \"out.vcf\"
                                {:file-date \"20090805\", :source \"myImpu...\" ...}
                                [\"CHROM\" \"POS\" \"ID\" \"REF\" \"ALT\" ...])]
      (WRITING-VCF))"
  ^VCFWriter
  [f meta-info header]
  (doto (VCFWriter. (util/as-url f)
                    (cio/writer (util/compressor-output-stream f))
                    meta-info
                    header)
    (vcf-writer/write-meta-info meta-info)
    (vcf-writer/write-header header)))

(defn bcf-writer
  "Returns an open cljam.io.bcf.writer.BCFWriter of f. Meta-information lines
  and a header line will be written in this function. Should be used inside
  with-open to ensure the writer is properly closed. e.g.

     (with-open [wtr (bcf-writer \"out.bcf\"
                                 {:file-date \"20090805\", :source \"myImpu...\" ...}
                                 [\"CHROM\" \"POS\" \"ID\" \"REF\" \"ALT\" ...])]
       (WRITING-BCF))"
  ^BCFWriter
  [f meta-info header]
  (bcf-writer/writer f meta-info header))

(defn writer
  "Selects suitable writer from f's extension, returning the open writer. This
  function supports VCF and BCF formats."
  ^Closeable
  [f meta-info header]
  (case (io-util/file-type f)
    :vcf (vcf-writer f meta-info header)
    :bcf (bcf-writer f meta-info header)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn write-variants
  "Writes variants to the VCF/BCF file. wtr must implement
  cljam.io.protocols/IVariantWriter. variants must be a sequence of parsed or
  VCF-style maps. e.g.

    (write-variants [{:chr \"19\", :pos 111, :id nil, :ref \"A\",
                      :alt [\"C\"], :qual 9.6, :filter [:PASS], :info {:DP 4},
                      :FORMAT [:GT :HQ] ...} ...])"
  [wtr variants]
  (protocols/write-variants wtr variants))

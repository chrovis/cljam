(ns cljam.io.vcf
  "Functions to read and write the VCF (Variant Call Format) format and BCF (its
  binary equivalent). See https://samtools.github.io/hts-specs/ for the detail
  VCF/BCF specifications."
  (:require [clojure.java.io :as cio]
            [cljam.util :as util]
            [cljam.io.protocols :as protocols]
            [cljam.io.util :as io-util]
            [cljam.io.vcf.reader :as vcf-reader]
            [cljam.io.vcf.writer :as vcf-writer]
            [cljam.io.bcf.reader :as bcf-reader]
            [cljam.io.bcf.writer :as bcf-writer])
  (:import java.io.Closeable
           cljam.io.vcf.reader.VCFReader
           cljam.io.vcf.writer.VCFWriter
           cljam.io.bcf.reader.BCFReader
           cljam.io.bcf.writer.BCFWriter))

;; Reading
;; -------

(defn ^VCFReader vcf-reader
  "Returns an open cljam.io.vcf.reader.VCFReader of f. Should be used inside
  with-open to ensure the reader is properly closed."
  [f]
  (let [meta-info (with-open [r (cio/reader (util/compressor-input-stream f))]
                    (vcf-reader/load-meta-info r))
        header (with-open [r (cio/reader (util/compressor-input-stream f))]
                 (vcf-reader/load-header r))]
    (VCFReader. (util/as-url f) meta-info header
                (cio/reader (util/compressor-input-stream f)))))

(defn ^BCFReader bcf-reader
  "Returns an open cljam.io.bcf.reader.BCFReader of f. Should be used inside
  with-open to ensure the reader is properly closed. Throws IOException if
  failed to parse BCF file format."
  [f]
  (bcf-reader/reader f))

(defn ^Closeable reader
  "Selects suitable reader from f's extension, returning the open reader. This
  function supports VCF and BCF formats."
  [f]
  (case (io-util/file-type f)
    :vcf (vcf-reader f)
    :bcf (bcf-reader f)
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn meta-info
  "Returns meta-info section of VCF/BCF file as a map."
  [rdr]
  (protocols/meta-info rdr))

(defn header
  "Returns header of VCF/BCF file as a sequence of strings."
  [rdr]
  (protocols/header rdr))

(defn read-variants
  "Reads variants of the VCF/BCF file, returning them as a lazy sequence. rdr
  must implement cljam.io.protocols/IVariantReader. Can take a option :depth to
  specify parsing level, default :deep. <:deep|:vcf|:bcf|:shallow|:raw>
    :deep - Fully parsed variant map. FORMAT, FILTER, INFO and samples columns are parsed.
    :vcf - VCF-style map. FORMAT, FILTER, INFO and samples columns are strings.
    :bcf - BCF-style map. CHROM, FILTER, INFO and :genotype contains indices to meta-info.
    :shallow - Only CHROM, POS and ref-length are parsed.
    :raw - Raw map of ByteBufers."
  ([rdr] (protocols/read-variants rdr))
  ([rdr option] (protocols/read-variants rdr option)))

;; Writing
;; -------

(defn ^VCFWriter vcf-writer
  "Returns an open cljam.io.vcf.writer.VCFWriter of f. Meta-information lines
  and a header line will be written in this function. Should be used inside
  with-open to ensure the writer is properly closed. e.g.

    (with-open [wtr (vcf-writer \"out.vcf\"
                                {:file-date \"20090805\", :source \"myImpu...\" ...}
                                [\"CHROM\" \"POS\" \"ID\" \"REF\" \"ALT\" ...])]
      (WRITING-VCF))"
  [f meta-info header]
  (doto (VCFWriter. (util/as-url f)
                    (cio/writer (util/compressor-output-stream f))
                    meta-info
                    header)
    (vcf-writer/write-meta-info meta-info)
    (vcf-writer/write-header header)))

(defn ^BCFWriter bcf-writer
  "Returns an open cljam.io.bcf.writer.BCFWriter of f. Meta-information lines
  and a header line will be written in this function. Should be used inside
  with-open to ensure the writer is properly closed. e.g.

     (with-open [wtr (bcf-writer \"out.bcf\"
                                 {:file-date \"20090805\", :source \"myImpu...\" ...}
                                 [\"CHROM\" \"POS\" \"ID\" \"REF\" \"ALT\" ...])]
       (WRITING-BCF))"
  [f meta-info header]
  (bcf-writer/writer f meta-info header))

(defn ^Closeable writer
  "Selects suitable writer from f's extension, returning the open writer. This
  function supports VCF and BCF formats."
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

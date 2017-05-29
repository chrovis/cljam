(ns cljam.vcf
  "Functions to read and write the Variant Call Format (VCF)."
  (:require [clojure.java.io :as cio]
            [cljam.util :as util]
            [cljam.vcf.reader :as vcf-reader]
            [cljam.vcf.writer :as vcf-writer])
  (:import cljam.vcf.reader.VCFReader
           cljam.vcf.writer.VCFWriter))

;; Reading
;; -------

(defn ^VCFReader reader
  "Returns an open cljam.vcf.reader.VCFReader of f. Should be used inside
  with-open to ensure the Reader is properly closed."
  [f]
  (let [meta-info (with-open [r (cio/reader (util/compressor-input-stream f))]
                    (vcf-reader/load-meta-info r))
        header (with-open [r (cio/reader (util/compressor-input-stream f))]
                 (vcf-reader/load-header r))]
    (VCFReader. (.getAbsolutePath (cio/file f)) meta-info header
                (cio/reader (util/compressor-input-stream f)))))

(defn meta-info
  "Returns meta-information of the VCF from rdr as a map."
  [^VCFReader rdr]
  (.meta-info rdr))

(defn header
  "Returns header of the VCF from rdr as a vector including header field
  strings."
  [^VCFReader rdr]
  (.header rdr))

(defn read-variants
  "Returns data lines of the VCF from rdr as a lazy sequence of maps. rdr must
  implement cljam.vcf.reader.VCFReader.
  Returned format can be specified with option :depth. Default is :deep.

    :deep Fully parsed variant map. FORMAT, FILTER, INFO and samples columns are parsed.
    :vcf  VCF-style map. FORMAT, FILTER, INFO and samples columns are strings."
  ([rdr]
   (read-variants rdr {}))
  ([rdr {:keys [depth] :or {depth :deep}}]
   (vcf-reader/read-variants rdr {:depth depth})))

;; Writing
;; -------

(defn ^VCFWriter writer
  "Returns an open cljam.vcf.writer.VCFWriter of f. Meta-information lines and
  a header line will be written in this function. Should be used inside
  with-open to ensure the Writer is properly closed. e.g.

    (with-open [wtr (writer \"out.vcf\"
                            {:file-date \"20090805\", :source \"myImpu...\" ...}
                            [\"CHROM\" \"POS\" \"ID\" \"REF\" \"ALT\" ...])]
      (WRITING-VCF))"
  [f meta-info header]
  (doto (VCFWriter. (.getAbsolutePath (cio/file f))
                    (cio/writer (util/compressor-output-stream f))
                    meta-info
                    header)
    (vcf-writer/write-meta-info meta-info)
    (vcf-writer/write-header header)))

(defn write-variants
  "Writes data lines on wtr, returning nil. variants must be a sequence of
  parsed or VCF-style maps. e.g.

    (write-variants [{:chr \"19\", :pos 111, :id nil, :ref \"A\",
                      :alt [\"C\"], :qual 9.6, :filter nil, :info nil,
                      :FORMAT \"GT:HQ\"}])"
  [wtr variants]
  (vcf-writer/write-variants wtr variants))

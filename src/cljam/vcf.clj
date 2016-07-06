(ns cljam.vcf
  "Functions to read and write the Variant Call Format (VCF)."
  (:require [clojure.java.io :as io]
            [cljam.vcf.reader :as vcf-reader])
  (:import cljam.vcf.reader.VCFReader))

(defn ^VCFReader reader
  "Returns an open cljam.vcf.reader.VCFReader of f. Should be used inside
  with-open to ensure the Reader is properly closed."
  [f]
  (let [meta-info (with-open [r (io/reader f)]
                    (vcf-reader/load-meta-info r))
        header (with-open [r (io/reader f)]
                 (vcf-reader/load-header r))]
    (VCFReader. (.getAbsolutePath (io/file f)) meta-info header
                (io/reader f))))

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
  implement cljam.vcf.reader.VCFReader."
  [rdr]
  (vcf-reader/read-variants rdr))

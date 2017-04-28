(ns cljam.bcf
  "Functions to read and write the Binary variant Call Format (BCF)."
  (:require [cljam.bcf.reader :as bcf-reader]
            [cljam.bcf.writer :as bcf-writer])
  (:import [cljam.bcf.reader BCFReader]
           [cljam.bcf.writer BCFWriter]))

;; Reading
;; -------

(defn ^BCFReader reader
  "Returns an open cljam.bcf.reader.BCFReader of f. Should be used inside with-open to
   ensure the Reader is properly closed.
   Throws IOException if failed to parse BCF file format."
  [f]
  (bcf-reader/reader f))

(defn meta-info
  "Returns meta-information of the BCF from rdr as a map."
  [^BCFReader rdr]
  (-> (.meta-info rdr)
      (update :contig (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))
      (update :filter (fn [xs] (keep (fn [m] (when-not (= (:id m) "PASS") (dissoc m :idx))) xs)))
      (update :info (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))
      (update :format (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))))

(defn header
  "Returns header of the BCF from rdr as a vector including header field strings."
  [^BCFReader rdr]
  (.header rdr))

(defn read-variants
  "Returns data lines of the BCF from rdr as a lazy sequence of maps.
   rdr must implement cljam.bcf.BCFReader.
   Can take a option :depth to specify parsing level. Default is :deep.

     :deep    Fully parsed variant map. FORMAT, FILTER, INFO and samples columns are parsed.
     :vcf     VCF-style map. FORMAT, FILTER, INFO and samples columns are strings.
     :bcf     BCF-style map. CHROM, FILTER, INFO and :genotype contains indices to meta-info.
     :shallow Only CHROM, POS and ref-length are parsed.
     :raw     Raw map of ByteBufers."
  [rdr & {:keys [depth] :or {depth :deep}}]
  (bcf-reader/read-variants rdr :depth depth))

;; Writing
;; -------

(defn ^BCFWriter writer
  "Returns an open cljam.bcf.BCFWriter of f.
   Meta-information lines and a header line will be written in this function.
   Should be used inside with-open to ensure the Writer is properly closed. e.g.

     (with-open [wtr (writer \"out.bcf\"
                             {:file-date \"20090805\", :source \"myImpu...\" ...}
                             [\"CHROM\" \"POS\" \"ID\" \"REF\" \"ALT\" ...])]
       (WRITING-BCF))"
  [f meta-info header]
  (bcf-writer/writer f meta-info header))

(defn write-variants
  "Writes data lines on writer, returning nil. variants must be a sequence of parsed or VCF-style maps. e.g.

    (write-variants [{:chr \"19\", :pos 111, :id nil, :ref \"A\",
                      :alt [\"C\"], :qual 9.6, :filter [:PASS], :info {:DP 4},
                      :FORMAT [:GT :HQ] ...} ...])"
  [wtr variants]
  (bcf-writer/write-variants wtr variants))

(ns cljam.io.sam.util
  "Utilities related to SAM/BAM format."
  (:require [clojure.string :as cstr]
            cljam.io.protocols
            [cljam.io.sam.util.cigar :as cigar]
            [cljam.io.util.bin :as util-bin]
            [cljam.io.sam.util.option :as opt])
  (:import [java.nio CharBuffer ByteBuffer]
           [java.nio.charset StandardCharsets]
           [cljam.io.protocols SAMAlignment]))

(def ^:const linear-index-shift 14)
(def ^:const linear-index-depth 5)
;;; parse

(defn parse-alignment
  "Parse an alignment line, returning a map of the alignment."
  [line]
  (let [[qname flag rname pos-str mapq cigar rnext pnext tlen seq qual & options] (cstr/split line #"\t")
        pos (Integer/parseInt pos-str)
        ref-length (cigar/count-ref cigar)
        end (if (zero? ref-length) 0 (int (dec (+ pos ref-length))))]
    (SAMAlignment. qname (Integer/parseInt flag) rname pos end (Integer/parseInt mapq)
                   cigar rnext (Integer/parseInt pnext) (Integer/parseInt tlen) (cstr/upper-case seq)
                   qual (map opt/parse-optional-field options))))

;;; stringify

(defn stringify-alignment [a]
  (->> a
       ((juxt :qname :flag :rname :pos :mapq :cigar :rnext :pnext :tlen :seq :qual
              (comp opt/stringify-optional-fields :options)))
       (cstr/join \tab)
       cstr/trim))

;;; indexing bin

(defn get-end
  "Returns the end position in reference for the given alignment."
  [aln]
  (let [ref-length (cigar/count-ref (:cigar aln))]
    (if (zero? ref-length)
      (:pos aln)
      (dec (+ (:pos aln) ref-length)))))

(defn compute-bin
  "Returns indexing bin based on alignment start and end."
  [aln]
  (let [beg (:pos aln)
        end (get-end aln)]
    (util-bin/reg->bin beg end linear-index-shift linear-index-depth)))

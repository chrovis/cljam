(ns cljam.bam.writer
  (:use [cljam.io])
  (:require [clojure.string :refer [split]]
            [clojure.java.io :refer [file]]
            (cljam [sam :as sam]
                   [cigar :as cgr]
                   [lsb :as lsb]
                   [util :refer [reg->bin string->bytes normalize-bases ubyte
                                 fastq->phred
                                 bytes->compressed-bases
                                 make-refs ref-id]])
            (cljam.bam [common :refer [bam-magic fixed-block-size]]))
  (:import [java.io DataOutputStream IOException EOFException]
           [chrovis.bgzf4j BGZFOutputStream]))
;;
;; BAMWriter
;;

(deftype BAMWriter [f writer]
  java.io.Closeable
  (close [this]
    (.. this writer close)))

;;
;; write
;;

(def ^:private fixed-tag-size 3)
(def ^:private fixed-binary-array-tag-size 5)

(defn- get-pos [aln]
  (dec (:pos aln)))

(defn- get-end [aln]
  (dec
   (+ (:pos aln)
      (cgr/count-ref (:cigar aln)))))

(defn- get-l-seq [sam-alignment]
  (count (:seq sam-alignment)))

(defn- get-next-ref-id [sa refs]
  (condp = (:rnext sa)
    "*" -1
    "=" (if-let [id (ref-id refs (:rname sa))] id -1)
    (if-let [id (ref-id refs (:rnext sa))] id -1)))

(defn- get-ref-id [aln refs]
  (if-let [id (ref-id refs (:rname aln))] id -1))

(defn- get-next-pos [sam-alignment]
  (dec (:pnext sam-alignment)))

(defn- get-tlen [sam-alignment]
  (:tlen sam-alignment))

(defn- get-read-name [sam-alignment]
  (:qname sam-alignment))

(defn- encode-cigar-op [op]
  (case op
    \M (byte 0)
    \I (byte 1)
    \D (byte 2)
    \N (byte 3)
    \S (byte 4)
    \H (byte 5)
    \P (byte 6)
    \= (byte 7)
    \X (byte 8)))

(defn- encode-cigar [cigar]
  (map #(bit-or (bit-shift-left (first %) 4)
                (encode-cigar-op (second %)))
       (cgr/parse cigar)))

(defn- get-cigar [aln]
  (encode-cigar (:cigar aln)))

(defn- get-seq [sam-alignment]
  (bytes->compressed-bases (normalize-bases (string->bytes (:seq sam-alignment)))))

(defn- get-options-size [sam-alignment]
  (->> (map
        (fn [op]
          (let [[tag value] (first (seq op))]
            (+ fixed-tag-size
               (case (first (:type value))
                 \A 1
                 \i 4
                 \f 4
                 \Z (inc (count (:value value)))
                 \B (let [[array-type & array] (split (:value value) #",")]
                      (+ fixed-binary-array-tag-size
                         (* (count array)
                            (case (first array-type)
                              \c 1
                              \C 1
                              \s 2
                              \S 2
                              \i 4
                              \I 4
                              \f 4
                              0))))))))
        (:options sam-alignment))
       (reduce +)))

(defn- get-block-size [aln]
  (let [read-length (count (normalize-bases (string->bytes (:seq aln))))
        cigar-length (cgr/count-op (:cigar aln))]
    (+ fixed-block-size (count (:qname aln)) 1
       (* cigar-length 4)
       (int (/ (inc read-length) 2))
       read-length
       (get-options-size aln))))

(defn- encode-qual [sam-alignment]
  (if (= (:qual sam-alignment) "*")
    (byte-array (count (:seq sam-alignment)) (ubyte 0xff))
    (fastq->phred (:qual sam-alignment))))

(defn- write-tag-value [writer val-type value]
  (case val-type
    \A (lsb/write-char writer (char value))
    \i (lsb/write-int    writer (int value))
    \f (lsb/write-float  writer (float value))
    \Z (lsb/write-string writer value)
    ;; \H nil
    \B (let [[array-type & array] (split value #",")]
         (case (first array-type)
           \c nil
           \C nil
           \s nil
           \S (do
                (lsb/write-bytes writer (byte-array 1 (byte \S)))
                (lsb/write-int writer (count array))
                (doseq [v array]
                 (lsb/write-short writer (Short/parseShort v))))
           \i nil
           \I nil
           \f nil))))

(defn write-header* [^BAMWriter wtr header]
  (let [w (.writer wtr)
        refs (make-refs header)
        header-string (str (sam/stringify-header header) \newline)]
    (lsb/write-bytes w (.getBytes bam-magic)) ; magic
    (lsb/write-int w (count header-string))
    (lsb/write-string w header-string)))

(defn write-refs* [^BAMWriter wtr header]
  (let [w (.writer wtr)
        refs (make-refs header)]
    (lsb/write-int w (count refs))
    (doseq [ref refs]
      (lsb/write-int w (inc (count (:name ref))))
      (lsb/write-string w (:name ref))
      (lsb/write-bytes w (byte-array 1 (byte 0)))
      (lsb/write-int w (:len ref)))))

(defn- write-alignment [wrtr aln refs]
  ;; block_size
  (lsb/write-int wrtr (get-block-size aln))
  ;; refID
  (lsb/write-int wrtr (get-ref-id aln refs))
  ;; pos
  (lsb/write-int wrtr (get-pos aln))
  ;; bin_mq_nl
  (lsb/write-ubyte wrtr (short (inc (count (:qname aln)))))
  (lsb/write-ubyte wrtr (short (:mapq aln)))
  (lsb/write-ushort wrtr (reg->bin (:pos aln) (get-end aln)))
  ;; flag_nc
  (lsb/write-ushort wrtr (cgr/count-op (:cigar aln)))
  (lsb/write-ushort wrtr (:flag aln))
  ;; l_seq
  (lsb/write-int wrtr (get-l-seq aln))
  ;; next_refID
  (lsb/write-int wrtr (get-next-ref-id aln refs))
  ;; next_pos
  (lsb/write-int wrtr (get-next-pos aln))
  ;; tlen
  (lsb/write-int wrtr (get-tlen aln))
  ;; read_name
  (lsb/write-string wrtr (get-read-name aln))
  (lsb/write-bytes wrtr (byte-array 1 (byte 0)))
  ;; cigar
  (doseq [cigar (get-cigar aln)]
    (lsb/write-int wrtr cigar))
  ;; seq
  (lsb/write-bytes wrtr (get-seq aln))
  ;; qual
  (lsb/write-bytes wrtr (encode-qual aln))
  ;; options
  (doseq [op (:options aln)]
    (let [[tag value] (first (seq op))]
      (lsb/write-short wrtr (short (bit-or (bit-shift-left (byte (second (name tag))) 8)
                                           (byte (first (name tag))))))
      (lsb/write-bytes wrtr (.getBytes ^String (:type value)))
      (write-tag-value wrtr (first (:type value)) (:value value)))))

(defn write-alignments* [^BAMWriter wtr alns header]
  (let [w (.writer wtr)
        refs (make-refs header)]
    (doseq [a alns]
      (write-alignment w a refs))))

(defn write-blocks* [^BAMWriter wtr blocks]
  (let [w (.writer wtr)]
    (doseq [b blocks]
      (lsb/write-int w (:size b))
      (lsb/write-bytes w (:data b)))))

;;
;; public
;;

(defn writer [f]
  (->BAMWriter f (DataOutputStream. (BGZFOutputStream. (file f)))))

(extend-type BAMWriter
  ISAMWriter
  (writer-path [this]
    (.f this))
  (write-header [this header]
    (write-header* this header))
  (write-refs [this header]
    (write-refs* this header))
  (write-alignments [this alignments header]
    (write-alignments* this alignments header))
  (write-blocks [this blocks]
    (write-blocks* this blocks))
  (write-coordinate-blocks [this blocks]
    (write-blocks* this blocks)))

(ns cljam.bam.encoder
  "Encoder of BAM alignment blocks."
  (:require [clojure.string :refer [split]]
            (cljam [cigar :as cgr]
                   [lsb :as lsb]
                   [util :refer [string->bytes ubyte]])
            [cljam.util.sam-util :refer [reg->bin normalize-bases fastq->phred
                                         str->compressed-bases make-refs ref-id
                                         stringify-header]]
            [cljam.bam.common :refer [bam-magic fixed-block-size]])
  (:import [java.util Arrays]))

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
  (str->compressed-bases (:seq sam-alignment)))

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
  (let [read-length (count (:seq aln))
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

(defn- encode-tag-value [val-type value]
  (case val-type
    \A (let [bb (lsb/gen-byte-buffer)]
         (.putChar bb (char value))
         (Arrays/copyOfRange (.array bb) 0 1))
    \i (let [bb (lsb/gen-byte-buffer)]
         (.putInt bb (int value))
         (Arrays/copyOfRange (.array bb) 0 4))
    \f (let [bb (lsb/gen-byte-buffer)]
         (.putFloat bb (float value))
         (Arrays/copyOfRange (.array bb) 0 4))
    \Z (let [^String text value
             text-size (count text)
             buf (byte-array (inc text-size))]
         (.getBytes text 0 text-size buf 0)
         (aset-byte buf text-size 0)
         buf)
    ;; \H nil
    \B (let [[array-type & array] (split value #",")]
         (case (first array-type)
           \c nil
           \C nil
           \s nil
           \S (let [total-len (+ 1 4 (* 2 (count array)))
                    bb (lsb/gen-byte-buffer total-len)]
                (.put bb (byte-array 1 (byte \S)) 0 1)
                (.putInt bb (count array))
                (doseq [v array]
                  (.putShort bb (Short/parseShort v)))
                (Arrays/copyOfRange (.array bb) 0 total-len))
           \i nil
           \I nil
           \f nil))))

(defn encode
  [aln refs]
  [(get-block-size aln)
   (get-ref-id aln refs)
   (get-pos aln)
   (short (inc (count (:qname aln))))
   (short (:mapq aln))
   (reg->bin (:pos aln) (get-end aln))
   (cgr/count-op (:cigar aln))
   (:flag aln)
   (get-l-seq aln)
   (get-next-ref-id aln refs)
   (get-next-pos aln)
   (get-tlen aln)
   (get-read-name aln)
   (byte-array 1 (byte 0))
   (get-cigar aln)
   (get-seq aln)
   (encode-qual aln)
   (doall (map (fn [op]
                 (let [[tag value] (first (seq op))]
                   [(short (bit-or (bit-shift-left (byte (second (name tag))) 8)
                                   (byte (first (name tag)))))
                    (.getBytes ^String (:type value))
                    (encode-tag-value (first (:type value)) (:value value))]))
               (:options aln)))])

(defn write-encoded-alignment
  [w aln]
  ;; block_size
  (lsb/write-int w (nth aln 0))
  ;; refID
  (lsb/write-int w (nth aln 1))
  ;; pos
  (lsb/write-int w (nth aln 2))
  ;; bin_mq_nl
  (lsb/write-ubyte w (nth aln 3))
  (lsb/write-ubyte w (nth aln 4))
  (lsb/write-ushort w (nth aln 5))
  ;; flag_nc
  (lsb/write-ushort w (nth aln 6))
  (lsb/write-ushort w (nth aln 7))
  ;; l_seq
  (lsb/write-int w (nth aln 8))
  ;; next_refID
  (lsb/write-int w (nth aln 9))
  ;; next_pos
  (lsb/write-int w (nth aln 10))
  ;; tlen
  (lsb/write-int w (nth aln 11))
  ;; read_name
  (lsb/write-string w (nth aln 12))
  (lsb/write-bytes w (nth aln 13))
  ;; cigar
  (doseq [cigar (nth aln 14)]
    (lsb/write-int w cigar))
  ;; seq
  (lsb/write-bytes w (nth aln 15))
  ;; qual
  (lsb/write-bytes w (nth aln 16))
  ;; options
  (doseq [v (nth aln 17)]
    (lsb/write-short w (nth v 0))
    (lsb/write-bytes w (nth v 1))
    (when-not (nil? (nth v 2))
      (lsb/write-bytes w (nth v 2)))))

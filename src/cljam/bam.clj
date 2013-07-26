(ns cljam.bam
  (:use [cljam.util :only [string->bytes normalize-bases ubyte
                           fastq->phred phred->fastq
                           bytes->compressed-bases compressed-bases->chars]])
  (:require [clojure.string :as str]
            [cljam.binary-cigar-codec :as bcc])
  (:import [net.sf.samtools TextCigarCodec BinaryCigarCodec CigarElement CigarOperator]
           [java.nio ByteBuffer ByteOrder]
           java.util.Arrays))

(def fixed-block-size 32)

(def fixed-tag-size 3)

(def fixed-binary-array-tag-size 5)

(def bam-magic "BAM\1")

(defn count-cigar [sam-alignment]
  (.numCigarElements (.decode (TextCigarCodec/getSingleton) (:cigar sam-alignment))))

(defn- get-options-size [sam-alignment]
  (->> (map
        (fn [op]
          (let [[tag value] (first (seq op))]
            (+ fixed-tag-size
               (condp = (first (:type value))
                 \A 1
                 \i 4
                 \f 4
                 \Z (inc (count (:value value)))
                 \B (let [[array-type & array] (str/split (:value value) #",")]
                      (+ fixed-binary-array-tag-size
                         (* (count array)
                            (condp = (first array-type)
                              \c 1
                              \C 1
                              \s 2
                              \S 2
                              \i 4
                              \I 4
                              \f 4))))))))
        (:options sam-alignment))
       (reduce +)))

(defn get-block-size [sam-alignment]
  (let [read-length (count (normalize-bases (string->bytes (:seq sam-alignment))))
        cigar-length (count-cigar sam-alignment)]
    (+ fixed-block-size (count (:qname sam-alignment)) 1
       (* cigar-length 4)
       (int (/ (inc read-length) 2))
       read-length
       (get-options-size sam-alignment))))

(defn get-ref-id [sam-alignment] -1)

(defn get-pos [sam-alignment]
  (dec (:pos sam-alignment)))

(defn get-end [sam-alignment]
  (- (+ (:pos sam-alignment)
        (.getReferenceLength (.decode (TextCigarCodec/getSingleton) (:cigar sam-alignment))))
     1))

(defn get-l-seq [sam-alignment]
  (count (:seq sam-alignment)))

(defn get-next-ref-id [sam-alignment] -1)

(defn get-next-pos [sam-alignment]
  (dec (:pnext sam-alignment)))

(defn get-tlen [sam-alignment]
  (:tlen sam-alignment))

(defn get-read-name [sam-alignment]
  (:qname sam-alignment))

(defn- encode-cigar [cigar]
  (map-indexed (fn [idx _]
         (let [cigar-element (.getCigarElement cigar idx)
               op (CigarOperator/enumToBinary (.getOperator cigar-element))]
           (bit-or (bit-shift-left (.getLength cigar-element) 4) op)))
       (range (.numCigarElements cigar))))

(defn decode-cigar [cigar-bytes]
  (let [byte-buffer (ByteBuffer/wrap cigar-bytes)]
    (.order byte-buffer ByteOrder/LITTLE_ENDIAN)
    (str (bcc/decode byte-buffer))))

(defn get-cigar [sam-alignment]
  (let [cigar (.decode (TextCigarCodec/getSingleton) (:cigar sam-alignment))]
    (encode-cigar cigar)))

(defn get-seq [sam-alignment]
  (bytes->compressed-bases (normalize-bases (string->bytes (:seq sam-alignment)))))

(defn decode-seq [seq-bytes length]
  (apply str (compressed-bases->chars length seq-bytes 0)))

(defn encode-qual [sam-alignment]
  (if (= (:qual sam-alignment "*"))
    (byte-array (count (:seq sam-alignment)) (ubyte 0xff))
    (fastq->phred (:qual sam-alignment))))

(defn decode-qual [b]
  (if (Arrays/equals b (byte-array (count b) (ubyte 0xff)))
    "*"
    (phred->fastq b)))

(ns cljam.bam
  (:use [cljam.util :only [reg2bin fastq-to-phred bytes-to-compressed-bases string-to-bytes
                           normalize-bases]])
  (:import [net.sf.samtools TextCigarCodec CigarElement CigarOperator]))

(defn index [] nil)

(def fixed-block-size 32)

(def bam-magic "BAM\1")

(defn count-cigar [sam-alignment]
  (.numCigarElements (.decode (TextCigarCodec/getSingleton) (:cigar sam-alignment))))

(defn get-block-size [sam-alignment]
  (let [read-length (count (normalize-bases (string-to-bytes (:seq sam-alignment))))
        cigar-length (count-cigar sam-alignment)]
    (+ fixed-block-size (count (:qname sam-alignment)) 1
       (* cigar-length 4)
       (int (/ (inc read-length) 2))
       read-length)))

(defn get-ref-id [sam-alignment] -1)

(defn get-pos [sam-alignment]
  (- (:pos sam-alignment) 1))

(defn get-end [sam-alignment]
  (- (+ (:pos sam-alignment)
        (.getReferenceLength (.decode (TextCigarCodec/getSingleton) (:cigar sam-alignment))))
     1))

(defn get-bin-mq-nl [sam-alignment]
  (bit-or (bit-shift-left (reg2bin (:pos sam-alignment) (get-end sam-alignment)) 16)
          (bit-shift-left (:mapq sam-alignment) 8)
          (inc (count (:qname sam-alignment)))))

(defn get-flag-nc [sam-alignment]
  (bit-or (bit-shift-left (:flag sam-alignment) 16)
          (count-cigar sam-alignment)))

(defn get-l-seq [sam-alignment]
  (count (:seq sam-alignment)))

(defn get-next-ref-id [sam-alignment] -1)

(defn get-next-pos [sam-alignment]
  (- (:pnext sam-alignment) 1))

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

(defn get-cigar [sam-alignment]
  (let [cigar (.decode (TextCigarCodec/getSingleton) (:cigar sam-alignment))]
    (encode-cigar cigar)))

(defn get-seq [sam-alignment]
  (bytes-to-compressed-bases (normalize-bases (string-to-bytes (:seq sam-alignment)))))

(defn get-qual [sam-alignment]
  (fastq-to-phred (:qual sam-alignment)))

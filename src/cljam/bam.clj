(ns cljam.bam
  (:require [clojure.string :refer [split join]]
            [clojure.java.io :refer [file]]
            (cljam [sam :as sam]
                   [lsb :as lsb]
                   [binary-cigar-codec :as bcc]
                   [util :refer [string->bytes normalize-bases ubyte
                                 fastq->phred phred->fastq
                                 bytes->compressed-bases compressed-bases->chars]]))
  (:import java.util.Arrays
           (java.io DataInputStream IOException)
           (java.nio ByteBuffer ByteOrder)
           (net.sf.samtools TextCigarCodec BinaryCigarCodec CigarElement CigarOperator)
           (net.sf.samtools.util BlockCompressedInputStream)
           (cljam.sam SamHeader SamAlignment)))

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
                 \B (let [[array-type & array] (split (:value value) #",")]
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

(defn get-ref-id [sa refs]
  (if-let [id (sam/ref-id refs (:rname sa))] id -1))

(defn get-pos [sam-alignment]
  (dec (:pos sam-alignment)))

(defn get-end [sam-alignment]
  (dec
   (+ (:pos sam-alignment)
      (.. TextCigarCodec getSingleton (decode (:cigar sam-alignment)) getReferenceLength))))

(defn get-l-seq [sam-alignment]
  (count (:seq sam-alignment)))

(defn get-next-ref-id [sa refs]
  (condp = (:rnext sa)
    "*" -1
    "=" (if-let [id (sam/ref-id refs (:rname sa))] id -1)
    (if-let [id (sam/ref-id refs (:rnext sa))] id -1)))

(defn decode-next-ref-id [refs n rname]
  (cond
    (= n -1) "*"
    (= (sam/ref-name refs n) rname) "="
    :else (sam/ref-name refs n)))

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
  (let [cigar (.. TextCigarCodec getSingleton (decode (:cigar sam-alignment)))]
    (encode-cigar cigar)))

(defn get-seq [sam-alignment]
  (bytes->compressed-bases (normalize-bases (string->bytes (:seq sam-alignment)))))

(defn decode-seq [seq-bytes length]
  (join (compressed-bases->chars length seq-bytes 0)))

(defn encode-qual [sam-alignment]
  (if (= (:qual sam-alignment) "*")
    (byte-array (count (:seq sam-alignment)) (ubyte 0xff))
    (fastq->phred (:qual sam-alignment))))

(defn decode-qual [b]
  (if (Arrays/equals b (byte-array (count b) (ubyte 0xff)))
    "*"
    (phred->fastq b)))

;;; I/O

(deftype ^:private BamReader [header refs reader]
  java.io.Closeable
  (close [this] (.. this reader close)))

(defn- parse-header [header]
  (map #(sam/parse-header %) (split header #"\n")))

(defn reader [f]
  (let [rdr (DataInputStream. (BlockCompressedInputStream. (file f)))]
    (if-not (Arrays/equals (lsb/read-bytes rdr 4) (.getBytes bam-magic))
      (throw (IOException. "Invalid BAM file header")))
    (let [header (parse-header (lsb/read-string rdr (lsb/read-int rdr)))
          n-ref  (lsb/read-int rdr)
          refs   (loop [i n-ref, ret []]
                   (if (zero? i)
                     ret
                     (let [l-name (lsb/read-int rdr)
                           name   (lsb/read-string rdr l-name)
                           l-ref  (lsb/read-int rdr)]
                       (recur (dec i)
                              (conj ret {:name (subs name 0 (dec l-name))
                                         :len  l-ref})))))]
      (->BamReader header refs rdr))))

(defn read-header
  [^BamReader rdr]
  (.header rdr))

(defn- parse-option [bb]
  (let [a (.get bb)
        b (.get bb)
        tag (str (char b) (char a))
        tag-type (char (.get bb))]
    {(keyword tag)
     {:type  (str tag-type)
      :value (condp = tag-type
               \A (.get bb)
               \i (.getInt bb)
               \f (.getFloat bb)
               \Z nil                   ; todo
               ;; \H nil
               \B (let [array-type (char (.get bb))
                        length (.getInt bb)]
                    (->> (for [i (range length)]
                           (condp = array-type ; BinaryTagCodec.java
                             \c (int (.get bb))
                             \C (bit-and (int (.get bb)) 0xff)
                             \s (int (.getShort bb))
                             \S (bit-and (.getShort bb) 0xffff)
                             \i (.getInt bb)
                             \I nil     ; todo
                             \f (.getFloat bb)))
                         (cons array-type)
                         (join \,))))}}))

(defn- parse-options [rest]
  (let [bb (ByteBuffer/wrap rest)]
    (.order bb ByteOrder/LITTLE_ENDIAN)
    (loop [options []]
      (if-not (.hasRemaining bb)
        options
        (recur (conj options (parse-option bb)))))))

(defn- read-alignment [rdr refs]
  (let [block-size (lsb/read-int rdr)]
    (if (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size:" block-size))))
    (let [ref-id      (lsb/read-int rdr)
          rname       (if (= ref-id -1) "*" (:name (nth refs ref-id)))
          pos         (inc (lsb/read-int rdr))
          l-read-name (lsb/read-ubyte rdr)
          mapq        (lsb/read-ubyte rdr)
          bin         (lsb/read-ushort rdr)
          n-cigar-op  (lsb/read-ushort rdr)
          flag        (lsb/read-ushort rdr)
          l-seq       (lsb/read-int rdr)
          rnext       (decode-next-ref-id refs (lsb/read-int rdr) rname)
          pnext       (inc (lsb/read-int rdr))
          tlen        (lsb/read-int rdr)
          qname       (lsb/read-string rdr (dec (int l-read-name)))
          _           (lsb/read-bytes rdr 1)
          cigar       (decode-cigar (lsb/read-bytes rdr (* n-cigar-op 4)))
          seq         (decode-seq (lsb/read-bytes rdr (/ (inc l-seq) 2)) l-seq)
          qual        (decode-qual (lsb/read-bytes rdr (count seq)))
          rest        (lsb/read-bytes rdr (- block-size
                                             fixed-block-size
                                             (int l-read-name)
                                             (* n-cigar-op 4)
                                             (/ (inc l-seq) 2)
                                             (count seq)))
          options (parse-options rest)]
      (SamAlignment. qname flag rname pos mapq cigar rnext pnext tlen seq qual options))))

(defn read-alignments
  [^BamReader rdr]
  (when-not (zero? (.available (.reader rdr)))
    (cons (read-alignment (.reader rdr) (.refs rdr))
          (lazy-seq (read-alignments rdr)))))

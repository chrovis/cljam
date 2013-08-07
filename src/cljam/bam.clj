(ns cljam.bam
  (:refer-clojure :exclude [slurp spit])
  (:require [clojure.string :refer [split join]]
            [clojure.java.io :refer [file]]
            (cljam [sam :as sam]
                   [lsb :as lsb]
                   [binary-cigar-codec :as bcc]
                   [util :refer [reg->bin string->bytes normalize-bases ubyte
                                 fastq->phred phred->fastq
                                 bytes->compressed-bases compressed-bases->chars]]))
  (:import java.util.Arrays
           (java.io DataInputStream DataOutputStream IOException)
           (java.nio ByteBuffer ByteOrder)
           (net.sf.samtools TextCigarCodec BinaryCigarCodec CigarElement CigarOperator)
           (net.sf.samtools.util BlockCompressedInputStream BlockCompressedOutputStream)))

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

(defn get-ref-id [aln refs]
  (if-let [id (sam/ref-id refs (:rname aln))] id -1))

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

(defn reader [f]
  (let [rdr (DataInputStream. (BlockCompressedInputStream. (file f)))]
    (if-not (Arrays/equals (lsb/read-bytes rdr 4) (.getBytes bam-magic))
      (throw (IOException. "Invalid BAM file header")))
    (let [header (sam/parse-header (lsb/read-string rdr (lsb/read-int rdr)))
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
  [rdr]
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
      {:qname qname, :flag flag, :rname rname, :pos pos, :mapq  mapq,
       :cigar cigar, :rnext rnext, :pnext pnext, :tlen tlen, :seq seq,
       :qual qual, :options options})))

(defn read-alignments
  [rdr]
  (when-not (zero? (.available (.reader rdr)))
    (cons (read-alignment (.reader rdr) (.refs rdr))
          (lazy-seq (read-alignments rdr)))))

(defn- write-tag-value [writer val-type value]
  (condp = val-type
    \A (lsb/write-bytes  writer (char value))
    \i (lsb/write-int    writer (Integer/parseInt value))
    \f (lsb/write-float  writer (Float/parseFloat value))
    \Z (lsb/write-string writer value)
    ;; \H nil
    \B (let [[array-type & array] (split value #",")]
         (condp = (first array-type)
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

(defn writer [f]
  (DataOutputStream. (BlockCompressedOutputStream. f)))

(defn write-header [wrtr hdr]
  (lsb/write-bytes wrtr (.getBytes bam-magic)) ; magic
  (let [header (str (sam/stringify-header hdr) \newline)]
    (lsb/write-int wrtr (count header))
    (lsb/write-string wrtr header)))

(defn write-refs [wrtr refs]
  (lsb/write-int wrtr (count refs))
  (doseq [ref refs]
    (lsb/write-int wrtr (inc (count (:name ref))))
    (lsb/write-string wrtr (:name ref))
    (lsb/write-bytes wrtr (byte-array 1 (byte 0)))
    (lsb/write-int wrtr (:len ref))))

(defn write-alignment [wrtr aln refs]
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
  (lsb/write-ushort wrtr (count-cigar aln))
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
      (lsb/write-bytes wrtr (.getBytes (:type value)))
      (write-tag-value wrtr (first (:type value)) (:value value)))))

(defn write-alignments [wrtr alns refs]
  (doseq [a alns]
    (write-alignment wrtr a refs)))

(defn slurp
  "Opens a reader on bam-file and reads all its headers and alignments,
  returning a map about sam records."
  [f]
  (with-open [r (reader f)]
    {:header (read-header r)
     :alignments (vec (read-alignments r))}))

(defn spit
  "Opposite of slurp-bam. Opens bam-file with writer, writes sam headers and
  alignments, then closes the bam-file."
  [f sam]
  (with-open [w (writer f)]
    (let [refs (sam/make-refs (:header sam))]
      (write-header w (:header sam))
      (write-refs w refs)
      (write-alignments w (:alignments sam) refs))))

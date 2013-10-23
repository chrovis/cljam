(ns cljam.bam
  (:refer-clojure :exclude [slurp spit])
  (:require [clojure.string :refer [split join]]
            [clojure.java.io :refer [file]]
            [cljam [sam :as sam]
                   [cigar :as cgr]
                   [lsb :as lsb]
                   [util :refer [reg->bin string->bytes normalize-bases ubyte
                                 hex-string->bytes fastq->phred phred->fastq
                                 bytes->compressed-bases compressed-bases->chars]]])
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream IOException EOFException]
           [java.nio ByteBuffer ByteOrder]
           [cljam.lib.bam SAMSequenceDictionary SAMSequenceRecord BAMFileIndex]
           [chrovis.bgzf4j BGZFInputStream BGZFOutputStream]))

(def fixed-block-size 32)

(def fixed-tag-size 3)

(def fixed-binary-array-tag-size 5)

(def ^String bam-magic "BAM\1")

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
                              \f 4))))))))
        (:options sam-alignment))
       (reduce +)))

(defn get-block-size [aln]
  (let [read-length (count (normalize-bases (string->bytes (:seq aln))))
        cigar-length (cgr/count-op (:cigar aln))]
    (+ fixed-block-size (count (:qname aln)) 1
       (* cigar-length 4)
       (int (/ (inc read-length) 2))
       read-length
       (get-options-size aln))))

(defn get-ref-id [aln refs]
  (if-let [id (sam/ref-id refs (:rname aln))] id -1))

(defn get-pos [aln]
  (dec (:pos aln)))

(defn get-end [aln]
  (dec
   (+ (:pos aln)
      (cgr/count-ref (:cigar aln)))))

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

(defn- decode-cigar-op [op]
  (condp = op
    (byte 0) \M
    (byte 1) \I
    (byte 2) \D
    (byte 3) \N
    (byte 4) \S
    (byte 5) \H
    (byte 6) \P
    (byte 7) \=
    (byte 8) \X))

(defn- decode-cigar*
  [^ByteBuffer buf]
  (when (.hasRemaining buf)
    (let [b  (.getInt buf)
          op (bit-and b 0xf)
          n  (bit-shift-right b 4)]
      (concat [n (decode-cigar-op op)] (decode-cigar* buf)))))

(defn decode-cigar [cigar-bytes]
  (let [buf (ByteBuffer/wrap cigar-bytes)]
    (.order buf ByteOrder/LITTLE_ENDIAN)
    (apply str (decode-cigar* buf))))

(defn get-cigar [aln]
  (encode-cigar (:cigar aln)))

(defn get-seq [sam-alignment]
  (bytes->compressed-bases (normalize-bases (string->bytes (:seq sam-alignment)))))

(defn decode-seq [seq-bytes length]
  (join (compressed-bases->chars length seq-bytes 0)))

(defn encode-qual [sam-alignment]
  (if (= (:qual sam-alignment) "*")
    (byte-array (count (:seq sam-alignment)) (ubyte 0xff))
    (fastq->phred (:qual sam-alignment))))

(defn decode-qual [^bytes b]
  (if (Arrays/equals b (byte-array (count b) (ubyte 0xff)))
    "*"
    (phred->fastq b)))

;;; I/O

(def ^:private buffer-size (* 1024 128))

(deftype BAMReader [header refs reader index]
  java.io.Closeable
  (close [this]
    (.. this reader close)))

(defprotocol ISAMReader
  (header [this])
  (refs [this]))

(extend-type BAMReader
  ISAMReader
  (header [this]
    (.header this))
  (refs [this]
    (.refs this)))

(defn- make-sequence-dictionary
  [sequences]
  (new SAMSequenceDictionary
       (map (fn [s] (new SAMSequenceRecord (:SN s) (:LN s)))
            sequences)))

(deftype BAMIndex [f sequences]
  java.io.Closeable
  (close [this] (.. this f close)))

(defn bam-index
  [f header]
  (let [bai-f (str f ".bai")]
    (when-not (.exists (file bai-f))
      (throw (IOException. "Could not find BAM Index file")))
    (let [sequences (:SQ header)
          seq-dict (make-sequence-dictionary sequences)
          bai (new BAMFileIndex (file bai-f) seq-dict)]
      (->BAMIndex bai sequences))))

(defn reader [f]
  (let [rdr (BGZFInputStream. (file f))
        data-rdr (DataInputStream. rdr)]
    (when-not (Arrays/equals ^bytes (lsb/read-bytes data-rdr 4) (.getBytes bam-magic))
      (throw (IOException. "Invalid BAM file header")))
    (let [header (sam/parse-header (lsb/read-string data-rdr (lsb/read-int data-rdr)))
          n-ref  (lsb/read-int data-rdr)
          refs   (loop [i n-ref, ret []]
                   (if (zero? i)
                     ret
                     (let [l-name (lsb/read-int data-rdr)
                           name   (lsb/read-string data-rdr l-name)
                           l-ref  (lsb/read-int data-rdr)]
                       (recur (dec i)
                              (conj ret {:name (subs name 0 (dec l-name))
                                         :len  l-ref})))))
          index (bam-index f header)]
      (->BAMReader header refs rdr index))))

(defn- options-size
  [block-size l-read-name n-cigar-op l-seq]
  (- block-size
     fixed-block-size
     (int l-read-name)
     (* n-cigar-op 4)
     (int (/ (inc l-seq) 2))
     l-seq))

(defn- parse-tag-single [tag-type ^ByteBuffer bb]
  (case tag-type
    \Z (lsb/read-null-terminated-string bb)
    \A (.get bb)
    \I (bit-and (.getInt bb) 0xffffffff)
    \i (.getInt bb)
    \s (int (.getShort bb))
    \S (bit-and (.getShort bb) 0xffff)
    \c (int (.get bb))
    \C (bit-and (int (.get bb)) 0xff)
    \f (.getFloat bb)
    \H (hex-string->bytes (lsb/read-null-terminated-string bb))
    (throw (Exception. "Unrecognized tag type"))))

(defn- parse-tag-array [^ByteBuffer bb]
  (let [typ (char (.get bb))
        len (.getInt bb)]
    (->> (for [i (range len)]
           (case typ
             \c (int (.get bb))
             \C (bit-and (int (.get bb)) 0xff)
             \s (int (.getShort bb))
             \S (bit-and (.getShort bb) 0xffff)
             \i (.getInt bb)
             \I (bit-and (.getInt bb) 0xffffffff)
             \f (.getFloat bb)
             (throw (Exception. (str "Unrecognized tag array type: " typ)))))
         (cons typ)
         (join \,))))

(defn- parse-option [^ByteBuffer bb]
  (let [tag (str (char (.get bb)) (char (.get bb)))
        typ (char (.get bb))]
    {(keyword tag) {:type  (str typ)
                    :value (if (= typ \B)
                             (parse-tag-array bb)
                             (parse-tag-single typ bb))}}))

(defn- parse-options [rest]
  (let [bb (ByteBuffer/wrap rest)]
    (.order bb ByteOrder/LITTLE_ENDIAN)
    (loop [options []]
      (if-not (.hasRemaining bb)
        options
        (recur (conj options (parse-option bb)))))))

(defn- read-alignment [^DataInputStream rdr refs]
  (let [^Integer block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
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
          qual        (decode-qual (lsb/read-bytes rdr l-seq))
          rest        (lsb/read-bytes rdr (options-size block-size
                                                        l-read-name
                                                        n-cigar-op
                                                        l-seq))
          options     (parse-options rest)]
      {:qname qname, :flag flag, :rname rname, :pos pos, :mapq  mapq,
       :cigar cigar, :rnext rnext, :pnext pnext, :tlen tlen, :seq seq,
       :qual qual, :options options})))

(defn- light-read-alignment [^DataInputStream rdr refs]
  (let [^Integer block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size:" block-size))))
    (let [ref-id      (lsb/read-int rdr)
          rname       (if (= ref-id -1) "*" (:name (nth refs ref-id)))
          pos         (inc (lsb/read-int rdr))
          l-read-name (lsb/read-ubyte rdr)
          ;mapq        (lsb/read-ubyte rdr)
          ;bin         (lsb/read-ushort rdr)
          _           (lsb/skip rdr 3)
          n-cigar-op  (lsb/read-ushort rdr)
          ;flag        (lsb/read-ushort rdr)
          _           (lsb/skip rdr 2)
          l-seq       (lsb/read-int rdr)
          ;rnext       (decode-next-ref-id refs (lsb/read-int rdr) rname)
          ;pnext       (inc (lsb/read-int rdr))
          ;tlen        (lsb/read-int rdr)
          _           (lsb/skip rdr 12)
          qname       (lsb/skip rdr (dec (int l-read-name)))
          _           (lsb/skip rdr 1)
          cigar       (decode-cigar (lsb/read-bytes rdr (* n-cigar-op 4)))
          seq         (lsb/skip rdr (/ (inc l-seq) 2))
          lqual       (lsb/skip rdr l-seq)
          rest        (lsb/skip rdr (options-size block-size
                                                  l-read-name
                                                  n-cigar-op
                                                  l-seq))
          ;options     (parse-options rest)
          ]
      {:rname rname, :pos pos, :cigar cigar})))

(defn- write-tag-value [writer val-type value]
  (case val-type
    \A (lsb/write-bytes  writer (char value))
    \i (lsb/write-int    writer (Integer/parseInt value))
    \f (lsb/write-float  writer (Float/parseFloat value))
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

(defn writer [f]
  (DataOutputStream. (BGZFOutputStream. (file f))))

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

(defn write-alignments [wrtr alns refs]
  (doseq [a alns]
    (write-alignment wrtr a refs)))

(defn- get-sequence-index
  [^BAMIndex bai ^String chr]
  (let [sequences (.sequences bai)
        indexed (map-indexed vector sequences)
        filtered (filter #(= (:SN (second %)) chr) indexed)
        idx (first (map first filtered))]
    (if (nil? idx) -1 idx)))

(defn get-spans
  [^BAMIndex bai
   ^String chr ^Long start ^Long end]
  (let [seq-index (get-sequence-index bai chr)
        span-array (.getSpanOverlapping (.f bai) seq-index start end)
        spans (partition 2 (.toCoordinateArray span-array))]
    spans))

(defn- read-to-finish
  [^BAMReader rdr
   ^Long finish
   ^clojure.lang.IFn read-fn]
  (when (and (not (zero? (.available (.reader rdr))))
             (> finish (.getFilePointer (.reader rdr))))
    (cons (read-fn (DataInputStream. (.reader rdr)) (.refs rdr))
          (lazy-seq (read-to-finish rdr finish read-fn)))))

(defn read-alignments*
  [^BAMReader rdr
   ^String chr ^Long start ^Long end
   ^clojure.lang.Keyword light-or-heavy]
  (let [^BAMIndex bai (.index rdr)
        spans (get-spans bai chr start end)
        window (fn [^clojure.lang.PersistentHashMap a]
                 (let [^Long left (:pos a)
                       ^Long right (+ left (cgr/count-ref (:cigar a)))]
                   (and (= chr (:rname a))
                        (<= start right)
                        (>= end left))))
        read-fn (cond
                 (= :light light-or-heavy) light-read-alignment
                 :else read-alignment)
        candidates (flatten (map (fn [[^Long begin ^Long finish]]
                                   (.seek (.reader rdr) begin)
                                   (doall (read-to-finish rdr finish read-fn))) spans))]
    (filter window candidates)))

(defn light-read-alignments
  [^BAMReader rdr
   ^String chr ^Long start ^Long end]
  (read-alignments* rdr chr start end :light))

(defn read-alignments
  [^BAMReader rdr
   ^String chr ^Long start ^Long end]
  (read-alignments* rdr chr start end :heavy))

(defn slurp
  "Opens a reader on bam-file and reads all its headers and alignments,
  returning a map about sam records."
  [f & options]
  (let [{:keys [chr start end] :or {chr nil
                                    start 0
                                    end -1}} options]
    (with-open [r (reader f)]
      (let [h (header r)]
        {:header h
         :alignments (if (nil? chr)
                       nil
                       (vec (read-alignments r chr start end)))}))))

(defn spit
  "Opposite of slurp-bam. Opens bam-file with writer, writes sam headers and
  alignments, then closes the bam-file."
  [f sam]
  (with-open [w (writer f)]
    (let [refs (sam/make-refs (:header sam))]
      (write-header w (:header sam))
      (write-refs w refs)
      (write-alignments w (:alignments sam) refs))))

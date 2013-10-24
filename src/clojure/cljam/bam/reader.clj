(ns cljam.bam.reader
  (:require [clojure.string :refer [join]]
            [clojure.java.io :refer [file]]
            (cljam [sam :as sam]
                   [protocol :refer [ISAMReader]]
                   [cigar :as cgr]
                   [lsb :as lsb]
                   [util :refer [string->bytes ubyte
                                 hex-string->bytes phred->fastq
                                 compressed-bases->chars]])
            (cljam.bam [index :refer [bam-index get-spans get-sequence-index]]
                       [common :refer [bam-magic fixed-block-size]]
                       [util :refer :all]))
  (:import java.util.Arrays
           [java.io DataInputStream IOException EOFException]
           [java.nio ByteBuffer ByteOrder]
           [chrovis.bgzf4j BGZFInputStream]))

;;
;; BAMReader
;;

(deftype BAMReader [header refs reader index]
  java.io.Closeable
  (close [this]
    (.. this reader close)))

(extend-type BAMReader
  ISAMReader
  (read-header [this]
    (.header this))
  (read-refs [this]
    (.refs this)))

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
          index (try (bam-index f header)
                     (catch IOException e nil))]
      (->BAMReader header refs rdr index))))

;;
;; read alignment
;;

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

(defn- options-size
  [block-size l-read-name n-cigar-op l-seq]
  (- block-size
     fixed-block-size
     (int l-read-name)
     (* n-cigar-op 4)
     (int (/ (inc l-seq) 2))
     l-seq))

(defn- decode-qual [^bytes b]
  (if (Arrays/equals b (byte-array (count b) (ubyte 0xff)))
    "*"
    (phred->fastq b)))

(defn- decode-seq [seq-bytes length]
  (join (compressed-bases->chars length seq-bytes 0)))

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

(defn- decode-next-ref-id [refs n rname]
  (cond
    (= n -1) "*"
    (= (sam/ref-name refs n) rname) "="
    :else (sam/ref-name refs n)))

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
          _           (lsb/skip rdr 3)
          n-cigar-op  (lsb/read-ushort rdr)
          _           (lsb/skip rdr 2)
          l-seq       (lsb/read-int rdr)
          _           (lsb/skip rdr 12)
          qname       (lsb/skip rdr (dec (int l-read-name)))
          _           (lsb/skip rdr 1)
          cigar       (decode-cigar (lsb/read-bytes rdr (* n-cigar-op 4)))
          seq         (lsb/skip rdr (/ (inc l-seq) 2))
          lqual       (lsb/skip rdr l-seq)
          rest        (lsb/skip rdr (options-size block-size
                                                  l-read-name
                                                  n-cigar-op
                                                  l-seq))]
      {:rname rname, :pos pos, :cigar cigar})))

(defn- read-to-finish
  [^BAMReader rdr
   ^Long finish
   ^clojure.lang.IFn read-fn]
  (when (and (not (zero? (.available (.reader rdr))))
             (> finish (.getFilePointer (.reader rdr))))
    (cons (read-fn (DataInputStream. (.reader rdr)) (.refs rdr))
          (lazy-seq (read-to-finish rdr finish read-fn)))))

(defn- read-alignments*
  [^BAMReader rdr
   ^String chr ^Long start ^Long end
   ^clojure.lang.Keyword light-or-heavy]
  (when (nil? (.index rdr))
    (throw (Exception. ("BAM index not found"))))
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

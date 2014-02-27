(ns cljam.bam.reader
  (:use [cljam.io])
  (:require [clojure.string :refer [join]]
            [clojure.java.io :refer [file]]
            (cljam [cigar :as cgr]
                   [lsb :as lsb]
                   [util :refer [string->bytes ubyte hex-string->bytes ]])
            [cljam.util.sam-util :refer [phred->fastq ref-id ref-name
                                         compressed-bases->chars
                                         parse-header]]
            (cljam.bam [common :refer [bam-magic fixed-block-size]]
                       [util :refer :all])
            [cljam.bam-index :refer [get-spans]])
  (:import java.util.Arrays
           [java.io DataInputStream Closeable IOException EOFException]
           [java.nio ByteBuffer ByteOrder]
           [bgzf4j BGZFInputStream]))

;;
;; BAMReader
;;

(deftype BAMReader [f header refs reader data-reader index]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this))))

;;
;; read alignment
;;

(defn- validate-tag-type
  [t]
  (case t
    \I \i
    \s \i
    \S \i
    \c \i
    \C \i
    t))

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
    {(keyword tag) {:type  (str (validate-tag-type typ))
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

(def ^:private cigar-op-map
  (hash-map (byte 0) \M
            (byte 1) \I
            (byte 2) \D
            (byte 3) \N
            (byte 4) \S
            (byte 5) \H
            (byte 6) \P
            (byte 7) \=
            (byte 8) \X))

(defn- decode-cigar-op [op]
  (get cigar-op-map op))

(defn- decode-cigar*
  [^ByteBuffer buf]
  (when (.hasRemaining buf)
    (let [b  (.getInt buf)
          op (bit-and b 0xf)
          n  (bit-shift-right b 4)]
      (cons n (cons (decode-cigar-op op) (decode-cigar* buf))))))

(defn decode-cigar [cigar-bytes]
  (let [buf (ByteBuffer/wrap cigar-bytes)]
    (.order buf ByteOrder/LITTLE_ENDIAN)
    (apply str (decode-cigar* buf))))

(defn- decode-next-ref-id [refs n rname]
  (cond
    (= n -1) "*"
    (= (ref-name refs n) rname) "="
    :else (ref-name refs n)))

(defn- read-alignment [^BAMReader bam-reader refs]
  (let [rdr (.data-reader bam-reader)
        ^Integer block-size (lsb/read-int rdr)]
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

;; TODO: improve performance using ByteBuffer
(defn- light-read-alignment [^BAMReader bam-reader refs]
  (let [rdr (.data-reader bam-reader)
        ^Integer block-size (lsb/read-int rdr)]
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
          _           (lsb/skip rdr (options-size block-size
                                                  l-read-name
                                                  n-cigar-op
                                                  l-seq))]
      {:rname rname, :pos pos, :cigar cigar})))

;; TODO: improve performance using ByteBuffer
(defn- pointer-read-alignment [^BAMReader bam-reader refs]
  (let [rdr (.data-reader bam-reader)
        pointer-beg (.getFilePointer ^BGZFInputStream (.reader bam-reader))
        block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size: " block-size))))
    (let [ref-id      (lsb/read-int rdr)
          rname       (if (= ref-id -1) "*" (ref-name refs ref-id))
          pos         (inc (lsb/read-int rdr))
          l-read-name (lsb/read-ubyte rdr)
          _           (lsb/skip rdr 3)
          n-cigar-op  (lsb/read-ushort rdr)
          flag        (lsb/read-ushort rdr)
          l-seq       (lsb/read-int rdr)
          _           (lsb/skip rdr (+ 12 (int l-read-name)))
          cigar       (decode-cigar (lsb/read-bytes rdr (* n-cigar-op 4)))
          _           (lsb/skip rdr (+ (/ (inc l-seq) 2)
                                       l-seq
                                       (options-size block-size
                                                     l-read-name
                                                     n-cigar-op
                                                     l-seq)))
          pointer-end (.getFilePointer ^BGZFInputStream (.reader bam-reader))]
      {:flag flag, :rname rname, :pos pos,:cigar cigar,
       :meta {:chunk {:beg pointer-beg, :end pointer-end}}})))

(defn- read-coordinate-alignment-block [^BAMReader bam-reader refs]
  (let [rdr (.data-reader bam-reader)
        ^Integer block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size:" block-size))))
    (let [data (lsb/read-bytes rdr block-size)
          bb (doto (lsb/gen-byte-buffer block-size)
               (.put data)
               (.flip))
          ref-id (.getInt bb)
          pos (inc (.getInt bb))
          bb-get-char (fn [b]
                        (let [c (.get b)
                              cb (lsb/gen-byte-buffer)]
                          (.limit cb (.capacity cb))
                          (.put cb c)
                          (.put cb (byte 0))
                          (.flip cb)
                          (.getShort cb)))
          bb-skip (fn [b l]
                    (.position b (+ l (.position b))))
          bb-get-string (fn [b l]
                          (let [a (byte-array l)]
                            (.get b a 0 l)
                            (String. ^bytes a 0 0 l)))
          l-read-name (int (bb-get-char bb))
          _ (bb-skip bb 23)
          qname (bb-get-string bb (dec l-read-name))]
      {:size block-size
       :data data
       :qname qname
       :rname (if (= ref-id -1) "*" (:name (nth refs ref-id)))
       :pos pos})))

(defn- read-alignment-block [^BAMReader bam-reader refs]
  (let [rdr (.data-reader bam-reader)
        ^Integer block-size (lsb/read-int rdr)]
    (when (< block-size fixed-block-size)
      (throw (Exception. (str "Invalid block size:" block-size))))
    {:size block-size
     :data (lsb/read-bytes rdr block-size)}))

(defn- read-to-finish
  [^BAMReader rdr
   ^Long finish
   ^clojure.lang.IFn read-fn]
  (when (and (not (zero? (.available (.reader rdr))))
             (> finish (.getFilePointer (.reader rdr))))
    (cons (read-fn rdr (.refs rdr))
          (lazy-seq (read-to-finish rdr finish read-fn)))))

(defn read-alignments*
  [^BAMReader rdr
   ^String chr ^Long start ^Long end
   deep-or-shallow]
  (when (nil? (.index rdr))
    (throw (Exception. "BAM index not found")))
  (let [^BAMIndex bai (.index rdr)
        spans (get-spans bai (ref-id (.refs rdr) chr) start end)
        window (fn [^clojure.lang.PersistentHashMap a]
                 (let [^Long left (:pos a)
                       ^Long right (+ left (cgr/count-ref (:cigar a)))]
                   (and (= chr (:rname a))
                        (<= start right)
                        (>= end left))))
        read-fn (case deep-or-shallow
                  :shallow light-read-alignment
                  :deep read-alignment
                  :pointer pointer-read-alignment)
        candidates (flatten (map (fn [[^Long begin ^Long finish]]
                                   (.seek (.reader rdr) begin)
                                   (doall (read-to-finish rdr finish read-fn))) spans))]
    (filter window candidates)))

(defn read-alignments-sequentially*
  [^BAMReader rdr deep-or-shallow]
  (let [read-aln-fn (case deep-or-shallow
                      :shallow light-read-alignment
                      :deep read-alignment
                      :pointer pointer-read-alignment)
        read-fn (fn read-fn*
                  [^BAMReader r ^clojure.lang.PersistentVector refs]
                  (if-let [a (try (read-aln-fn r refs)
                                  (catch EOFException e nil))]
                    (cons a (lazy-seq (read-fn* r refs)))
                    nil))]
    (read-fn rdr (.refs rdr))))

(defn read-blocks-sequentially*
  [^BAMReader rdr option]
  (let [read-aln-fn (case option
                      :normal read-alignment-block
                      :coordinate read-coordinate-alignment-block)
        read-fn (fn read-fn* [^BAMReader r ^clojure.lang.PersistentVector refs]
                  (let [b (try (read-aln-fn r refs)
                               (catch EOFException e nil))]
                    (if b
                      (cons b (lazy-seq (read-fn* r refs)))
                      nil)))]
    (read-fn rdr (.refs rdr))))

(defn load-headers
  [rdr]
  (when-not (Arrays/equals ^bytes (lsb/read-bytes rdr 4) (.getBytes bam-magic))
    (throw (IOException. "Invalid BAM file header")))
  (let [header (parse-header (lsb/read-string rdr (lsb/read-int rdr)))
        n-ref (lsb/read-int rdr)
        refs (loop [i n-ref, ret []]
               (if (zero? i)
                 ret
                 (let [l-name (lsb/read-int rdr)
                       name   (lsb/read-string rdr l-name)
                       l-ref  (lsb/read-int rdr)]
                   (recur (dec i)
                          (conj ret {:name (subs name 0 (dec l-name))
                                     :len  l-ref})))))]
    {:header header
     :refs refs}))

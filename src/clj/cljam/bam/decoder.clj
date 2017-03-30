(ns cljam.bam.decoder
  "Decoder of BAM alignment blocks."
  (:require [clojure.string :refer [join]]
            [cljam.util :refer [ubyte hex-string->bytes]]
            [cljam.util.sam-util :refer [phred->fastq compressed-bases->str ref-name]]
            [cljam.lsb :as lsb]
            [cljam.bam.common :refer [fixed-block-size]])
  (:import java.util.Arrays
           [java.nio ByteBuffer ByteOrder]))

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

(defn decode-options [rest]
  (let [bb (ByteBuffer/wrap rest)]
    (.order bb ByteOrder/LITTLE_ENDIAN)
    (loop [options []]
      (if-not (.hasRemaining bb)
        options
        (recur (conj options (parse-option bb)))))))

(defn options-size
  [^long block-size ^long l-read-name ^long n-cigar-op ^long l-seq]
  (- block-size
     fixed-block-size
     l-read-name
     (* n-cigar-op 4)
     (int (/ (inc l-seq) 2))
     l-seq))

(defn decode-qual [^bytes b]
  (if (Arrays/equals b (byte-array (alength b) (ubyte 0xff)))
    "*"
    (phred->fastq b)))

(defn decode-seq [seq-bytes length]
  (compressed-bases->str length seq-bytes 0))

(defn decode-next-ref-id [refs n rname]
  (cond
    (= n -1) "*"
    (= (ref-name refs n) rname) "="
    :else (ref-name refs n)))

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

(defn decode-cigar
  [cigar-bytes]
  (let [buf (ByteBuffer/wrap cigar-bytes)]
    (.order buf ByteOrder/LITTLE_ENDIAN)
    (loop [sb (StringBuilder.)]
      (if (.hasRemaining buf)
        (let [b  (.getInt buf)
              op (bit-and b 0xF)
              n  (bit-shift-right b 4)]
          (recur (doto sb (.append n)
                          (.append (decode-cigar-op op)))))
        (str sb)))))

(defn deep-decode-alignment-block
  [block refs]
  (let [buffer (ByteBuffer/wrap (:data block))]
    (let [ref-id      (lsb/read-int buffer)
          rname       (or (ref-name refs ref-id) "*")
          pos         (inc (lsb/read-int buffer))
          l-read-name (int (lsb/read-ubyte buffer))
          mapq        (lsb/read-ubyte buffer)
          bin         (lsb/read-ushort buffer)
          n-cigar-op  (lsb/read-ushort buffer)
          flag        (lsb/read-ushort buffer)
          l-seq       (lsb/read-int buffer)
          rnext       (decode-next-ref-id refs (lsb/read-int buffer) rname)
          pnext       (inc (lsb/read-int buffer))
          tlen        (lsb/read-int buffer)
          qname       (lsb/read-string buffer (dec l-read-name))
          _           (lsb/skip buffer 1)
          cigar       (decode-cigar (lsb/read-bytes buffer (* n-cigar-op 4)))
          seq         (decode-seq (lsb/read-bytes buffer (quot (inc l-seq) 2)) l-seq)
          qual        (decode-qual (lsb/read-bytes buffer l-seq))
          rest        (lsb/read-bytes buffer (options-size (:size block)
                                                           l-read-name
                                                           n-cigar-op
                                                           l-seq))
          options     (decode-options rest)]
      {:qname qname, :flag flag, :rname rname, :pos pos, :mapq  mapq,
       :cigar cigar, :rnext rnext, :pnext pnext, :tlen tlen, :seq seq,
       :qual qual, :options options})))

(defn light-decode-alignment-block
  [block refs]
  (let [buffer (ByteBuffer/wrap (:data block))]
    (let [ref-id      (lsb/read-int buffer)
          rname       (or (ref-name refs ref-id) "*")
          pos         (inc (lsb/read-int buffer))
          l-read-name (int (lsb/read-ubyte buffer))
          _           (lsb/skip buffer 3)
          n-cigar-op  (lsb/read-ushort buffer)
          _           (lsb/skip buffer (+ 18 l-read-name))
          cigar-bytes (lsb/read-bytes buffer (* n-cigar-op 4))]
      {:rname rname, :pos pos, :meta {:cigar-bytes cigar-bytes}})))

(defn pointer-decode-alignment-block
  [block refs]
  (let [buffer (ByteBuffer/wrap (:data block))]
    (let [rname       (or (ref-name refs (lsb/read-int buffer)) "*")
          pos         (inc (lsb/read-int buffer))
          l-read-name (int (lsb/read-ubyte buffer))
          _           (lsb/skip buffer 3)
          n-cigar-op  (lsb/read-ushort buffer)
          flag        (lsb/read-ushort buffer)
          _           (lsb/skip buffer (+ 16 l-read-name))
          cigar-bytes (lsb/read-bytes buffer (* n-cigar-op 4))]
      {:flag flag, :rname rname, :pos pos,
       :meta {:chunk {:beg (:pointer-beg block),
                      :end (:pointer-end block)}
              :cigar-bytes cigar-bytes}})))

(defn decode-alignment-block
  [block refs depth]
  (let [f (case depth
            :deep deep-decode-alignment-block
            :shallow light-decode-alignment-block
            :pointer pointer-decode-alignment-block)]
    (f block refs)))

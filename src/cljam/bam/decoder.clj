(ns cljam.bam.decoder
  "Decoder of BAM alignment blocks."
  (:require [clojure.string :refer [join]]
            [cljam.util :refer [ubyte hex-string->bytes]]
            [cljam.util.sam-util :refer [phred-bytes->fastq compressed-bases->str ref-name]]
            [cljam.lsb :as lsb]
            [cljam.bam.common :refer [fixed-block-size]])
  (:import java.util.Arrays
           [java.nio ByteBuffer ByteOrder CharBuffer]))

(definline validate-tag-type
  [t]
  `(case (long ~t)
    ~(long \I) \i
    ~(long \s) \i
    ~(long \S) \i
    ~(long \c) \i
    ~(long \C) \i
    (char ~t)))

(definline parse-tag-single [tag-type ^ByteBuffer bb]
  `(case (long ~tag-type)
    ~(long \Z) (lsb/read-null-terminated-string ~bb)
    ~(long \A) (.get ~bb)
    ~(long \I) (bit-and (.getInt ~bb) 0xffffffff)
    ~(long \i) (.getInt ~bb)
    ~(long \s) (int (.getShort ~bb))
    ~(long \S) (bit-and (.getShort ~bb) 0xffff)
    ~(long \c) (int (.get ~bb))
    ~(long \C) (bit-and (int (.get ~bb)) 0xff)
    ~(long \f) (.getFloat ~bb)
    ~(long \H) (hex-string->bytes (lsb/read-null-terminated-string ~bb))
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
  (lazy-seq
   (when (.hasRemaining bb)
     (cons
      (let [cb (doto (CharBuffer/allocate 2)
                 (.put (unchecked-char (.get bb)))
                 (.put (unchecked-char (.get bb)))
                 (.flip))
            typ (.get bb)]
        {(keyword (.toString cb))
         {:type  (str (validate-tag-type typ))
          :value (if (= typ 66) ;; (byte \B)
                   (parse-tag-array bb)
                   (parse-tag-single typ bb))}})
      (parse-option bb)))))

(defn decode-options [rest]
  (let [bb (ByteBuffer/wrap rest)]
    (.order bb ByteOrder/LITTLE_ENDIAN)
    (parse-option bb)))

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
    (phred-bytes->fastq b)))

(defn decode-seq [seq-bytes length]
  (compressed-bases->str length seq-bytes 0))

(defn decode-next-ref-id [refs n rname]
  (cond
    (= n -1) "*"
    (= (ref-name refs n) rname) "="
    :else (ref-name refs n)))

(defn decode-cigar-and-ref-length
  [cigar-bytes]
  (let [buf (ByteBuffer/wrap cigar-bytes)
        sb (StringBuilder.)]
    (.order buf ByteOrder/LITTLE_ENDIAN)
    (loop [ref-length 0]
      (if (.hasRemaining buf)
        (let [b (.getInt buf)
              op (bit-and b 0xF)
              n  (bit-shift-right b 4)]
          (doto sb
            (.append n)
            (.append (case op 0 \M 1 \I 2 \D 3 \N 4 \S 5 \H 6 \P 7 \= 8 \X)))
          (recur (+ ref-length (case op 0 n 2 n 3 n 7 n 8 n 0))))
        [(.toString sb) ref-length]))))

(defrecord SAMAlignment [^String qname ^int flag ^String rname ^int pos ^int end ^int mapq ^String cigar
                         ^String rnext ^int pnext ^int tlen ^String seq ^String qual options])

(definline read-bytes [buf len]
  `(let [ba# (byte-array ~len)]
     (.get ~buf ba#)
     ba#))

(defn deep-decode-alignment-block
  [block refs]
  (let [buffer (ByteBuffer/wrap (:data block))]
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (let [ref-id      (.getInt buffer)
          rname       (or (ref-name refs ref-id) "*")
          pos         (inc (.getInt buffer))
          l-read-name (int (bit-and (.get buffer) 0xFF))
          mapq        (bit-and (.get buffer) 0xFF)
          bin         (bit-and (.getShort buffer) 0xFFFF)
          n-cigar-op  (bit-and (.getShort buffer) 0xFFFF)
          flag        (bit-and (.getShort buffer) 0xFFFF)
          l-seq       (.getInt buffer)
          next-ref-id (.getInt buffer)
          rnext       (if (= ref-id next-ref-id) "=" (or (ref-name refs next-ref-id) "*"))
          pnext       (inc (.getInt buffer))
          tlen        (.getInt buffer)
          qname       (String. (read-bytes buffer l-read-name) 0 (dec l-read-name))
          [cigar len] (decode-cigar-and-ref-length (read-bytes buffer (* n-cigar-op 4)))
          seq         (decode-seq (read-bytes buffer (quot (inc l-seq) 2)) l-seq)
          qual        (decode-qual (read-bytes buffer l-seq))
          rest        (read-bytes buffer (options-size (:size block) l-read-name n-cigar-op l-seq))
          options     (decode-options rest)]
      (SAMAlignment. qname (int flag) rname (int pos) (if (zero? len) 0 (int (dec (+ pos len)))) (int mapq)
                     cigar rnext (int pnext) (int tlen) seq qual options))))

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

(ns cljam.io
  (:refer-clojure :exclude [read-string slurp spit])
  (:use [clojure.java.io :only [reader writer file]]
        [cljam.util :only [reg->bin string->bytes]])
  (:require [clojure.string :as str]
            [cljam.sam :as sam]
            [cljam.bam :as bam])
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream IOException]
           [java.nio ByteBuffer ByteOrder]
           [net.sf.samtools.util BlockCompressedInputStream BlockCompressedOutputStream]
           [cljam.sam Sam SamHeader SamAlignment]))

(def byte-buffer (ByteBuffer/allocate 8))
(.order byte-buffer ByteOrder/LITTLE_ENDIAN)

(defn- read-bytes
  ([reader length]
     (let [buf (byte-array length)]
       (.read reader buf 0 length)
       buf))
  ([reader buffer offset length]
     (loop [total-read 0]
       (if (>= total-read length) nil
           (do
             (let [num-read (.read reader buffer (+ offset total-read) (- length total-read))]
               (if (neg? num-read)
                 (throw (Exception. "Premature EOF"))
                 (recur (+ total-read num-read)))))))))

(defn- read-byte-buffer [reader n]
  {:pre (< n (.capacity byte-buffer))}
  (read-bytes reader (.array byte-buffer) 0 n)
  (.limit byte-buffer (.capacity byte-buffer))
  (.position byte-buffer n))

(defn- read-ubyte [reader]
  (read-byte-buffer reader 1)
  (.put byte-buffer (byte 0))
  (.flip byte-buffer)
  (.getShort byte-buffer))

(defn- read-ushort [reader]
  (read-byte-buffer reader 2)
  (.putShort byte-buffer (short 0))
  (.flip byte-buffer)
  (.getInt byte-buffer))

(defn- cljam.io/read-string [reader length]
  (String. (read-bytes reader length) 0 0 length))

(defn- read-int [reader]
  (read-byte-buffer reader 4)
  (.flip byte-buffer)
  (.getInt byte-buffer))

(defn- write-int [writer value]
  (.clear byte-buffer)
  (.putInt byte-buffer value)
  (.write writer (.array byte-buffer) 0 4))

(defn- write-float [write value]
  (.clear byte-buffer)
  (.putFloat byte-buffer value)
  (.write writer (.array byte-buffer) 0 4))

(defn- write-short [writer value]
  (.clear byte-buffer)
  (.putShort byte-buffer value)
  (.write writer (.array byte-buffer) 0 2))

(defn- write-string [writer s]
  (let [data-bytes (string->bytes s)]
   (.write writer data-bytes 0 (count data-bytes))))

(defn- write-bytes [writer b]
  (.write writer b 0 (count b)))

(defn- write-ubyte [writer value]
  (.clear byte-buffer)
  (.putShort byte-buffer value)
  (.write writer (.array byte-buffer) 0 1))

(defn- write-ushort [writer value]
  (.clear byte-buffer)
  (.putInt byte-buffer value)
  (.write writer (.array byte-buffer) 0 2))

(defn slurp-sam
  "Opens a reader on sam-file and reads all its headers and alignments,
  returning a map about sam records."
  [sam-file]
  (with-open [r (reader sam-file)]
    (loop [sam (Sam. [] [])
           line (.readLine r)]
      (if (nil? line)
        sam
        (recur
         (if (= (first line) \@)
           (assoc sam :header (conj (:header sam) (sam/parse-header line)))
           (assoc sam :alignments (conj (:alignments sam) (sam/parse-alignment line))))
         (.readLine r))))))

(defn spit-sam
  "Opposite of slurp-sam. Opens sam-file with writer, writes sam headers and
  alignments, then closes the sam-file."
  [sam-file sam]
  (with-open [w (writer sam-file)]
    (doseq [sh (:header sam)]
      (.write w (sam/stringify sh))
      (.newLine w))
    (doseq [sa (:alignments sam)]
      (.write w (sam/stringify sa))
      (.newLine w))
    nil))

(defn- parse-header [header]
  (map #(sam/parse-header %) (str/split header #"\n")))

(defn- parse-alignments [r]
  (loop [alignments []]
    (if (zero? (.available r))
      alignments
      (do
        (let [block-size (read-int r)]
          ;; (println "block-size: " block-size)
          (if (< block-size bam/fixed-block-size)
            (throw (Exception. (str "Invalid block size:" block-size)))))
        (let [ref-id (read-int r)
              rname "todo"
              pos (inc (read-int r))
              l-read-name (read-ubyte r)
              mapq (read-ubyte r)
              bin (read-ushort r)
              n-cigar-op (read-ushort r)
              flag (read-ushort r)
              l-seq (read-int r)
              rnext (read-int r)
              pnext (inc (read-int r))
              tlen (read-int r)
              qname (cljam.io/read-string r (dec (int l-read-name)))
              _ (read-bytes r 1)
              cigar (bam/decode-cigar (read-bytes r (* n-cigar-op 4)))
              seq (bam/decode-seq (read-bytes r (/ (inc l-seq) 2)) l-seq)
              qual (read-bytes r (count seq))]
          (recur (conj alignments (SamAlignment. qname flag rname pos mapq cigar rnext pnext tlen seq qual nil))))))))

(defn slurp-bam
  "Opens a reader on bam-file and reads all its headers and alignments,
  returning a map about sam records."
  [bam-file]
  (with-open [r (DataInputStream. (BlockCompressedInputStream. (file bam-file)))]
    (if-not (Arrays/equals (read-bytes r 4) (.getBytes bam/bam-magic))
      (throw (IOException. "Invalid BAM file header")))
    (let [header (parse-header (cljam.io/read-string r (read-int r)))]
      (let [cnt (read-int r)]
        (loop [i cnt]
          (if (zero? i)
            nil
            (do (cljam.io/read-string r  (read-int r))
                (read-int r)
                (recur (dec i))))))
      (Sam. header (parse-alignments r)))))

(defn- stringify-header [headers]
  (->> (map #(sam/stringify %) headers)
       (str/join \newline)))

(defn- write-tag-value [writer val-type value]
  (condp = val-type
    \A (write-bytes  writer (char value))
    \i (write-int    writer (Integer/parseInt value))
    \f (write-float  writer (Float/parseFloat value))
    \Z (write-string writer value)
    ;; \H nil
    \B (let [[array-type & array] (str/split value #",")]
         (condp = (first array-type)
           \c nil
           \C nil
           \s nil
           \S (do
                (write-bytes writer (byte-array 1 (byte \S)))
                (write-int writer (count array))
                (doseq [v array]
                 (write-short writer (Short/parseShort v))))
           \i nil
           \I nil
           \f nil))))

(defn spit-bam
  "Opposite of slurp-bam. Opens bam-file with writer, writes sam headers and
  alignments, then closes the bam-file."
  [bam-file sam]
  (with-open [w (DataOutputStream. (BlockCompressedOutputStream. bam-file))]
    ;; header
    (write-bytes w (.getBytes bam/bam-magic)) ; magic
    (let [header (str (stringify-header (:header sam)) \newline)]
      (write-int w (count header))
      (write-string w header))
    (write-int w (count (:header sam)))

    ;; list of reference information
    (doseq [sh (:header sam)]
      (write-int w (inc (count (:SN (:SQ sh)))))
      (write-string w (:SN (:SQ sh)))
      (write-bytes w (byte-array 1 (byte 0)))
      (write-int w (Integer/parseInt (:LN (:SQ sh)))))

    ;; list of alignments
    (doseq [sa (:alignments sam)]
      (write-int w (bam/get-block-size sa))

      (write-int w (bam/get-ref-id sa))

      (write-int w (bam/get-pos sa))

      (write-ubyte w (short (inc (count (:qname sa)))))
      (write-ubyte w (short (:mapq sa)))
      (write-ushort w (reg->bin (:pos sa) (bam/get-end sa)))

      (write-ushort w (bam/count-cigar sa))
      (write-ushort w (:flag sa))

      (write-int w (bam/get-l-seq sa))

      (write-int w (bam/get-next-ref-id sa))

      (write-int w (bam/get-next-pos sa))

      (write-int w (bam/get-tlen sa))

      (write-string w (bam/get-read-name sa))
      (write-bytes w (byte-array 1 (byte 0)))

      (doseq [cigar (bam/get-cigar sa)]
        (write-int w cigar))

      (write-bytes w (bam/get-seq sa))

      (write-bytes w (bam/get-qual sa))
;; (short)(tag.charAt(1) << 8 | tag.charAt(0));
      ;; options
      (doseq [op (:options sa)]
        (let [[tag value] (first (seq op))]
          (write-short w (short (bit-or (bit-shift-left (byte (second (name tag))) 8)
                                        (byte (first (name tag))))))
          (write-bytes w (.getBytes (:type value)))
          (write-tag-value w (first (:type value)) (:value value)))))
    nil))

(defn slurp
  "Opens a reader on sam/bam-file and reads all its headers and alignments,
  returning a map about sam records."
  [f]
  (condp #(re-find %1 %2) f
      #"\.sam$" (slurp-sam f)
      #"\.bam$" (slurp-bam f)
      (throw (IllegalArgumentException. "Invalid file type"))))

(defn spit
  "Opposite of slurp. Opens sam/bam-file with writer, writes sam headers and
  alignments, then closes the sam/bam-file."
  [f sam]
  (condp #(re-find %1 %2) f
      #"\.sam$" (spit-sam f sam)
      #"\.bam$" (spit-bam f sam)
      (throw (IllegalArgumentException. "Invalid file type"))))

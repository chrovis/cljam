(ns cljam.io
  (:refer-clojure :exclude [slurp spit])
  (:use [clojure.java.io :only [reader writer file]]
        [cljam.util :only [reg->bin string->bytes]])
  (:require [clojure.string :as str]
            [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.lsb :as lsb]
            [cljam.indexer :as indexer])
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream RandomAccessFile IOException]
           [java.nio ByteBuffer ByteOrder]
           [net.sf.samtools.util BlockCompressedInputStream BlockCompressedOutputStream]
           [cljam.sam Sam SamHeader SamAlignment]))

;;; sam

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

;;; bam

(defn- parse-header [header]
  (map #(sam/parse-header %) (str/split header #"\n")))

(defn- parse-option [bb]
  (let [a (.get bb)
        b (.get bb)
        tag (str (char b) (char a))
        tag-type (char (.get bb))]
    {(keyword tag)
     {:type  tag-type
      :value (condp = tag-type
               \A (.get bb)
               \i (.getInt bb)
               \f (.getFloat bb)
               \Z nil                   ; todo
               ;; \H nil
               \B (let [array-type (char (.get bb))
                        length (.getInt bb)]
                    (->> (for [i (range length)]
                           (condp = array-type
                             \c nil     ; todo
                             \C nil     ; todo
                             \s nil     ; todo
                             \S (.getShort bb)
                             \i nil     ; todo
                             \I nil     ; todo
                             \f nil))   ; todo
                         (cons array-type)
                         (str/join \,))))}}))

(defn- parse-options [rest]
  (let [bb (ByteBuffer/wrap rest)]
    (.order bb ByteOrder/LITTLE_ENDIAN)
    (loop [options []]
      (if-not (.hasRemaining bb)
        options
        (recur (conj options (parse-option bb)))))))

(defn- parse-alignments [r]
  (loop [alignments []]
    (if (zero? (.available r))
      alignments
      (do
        (let [block-size (lsb/read-int r)]
          (if (< block-size bam/fixed-block-size)
            (throw (Exception. (str "Invalid block size:" block-size))))
          (let [ref-id (lsb/read-int r)
                rname "todo"
                pos (inc (lsb/read-int r))
                l-read-name (lsb/read-ubyte r)
                mapq (lsb/read-ubyte r)
                bin (lsb/read-ushort r)
                n-cigar-op (lsb/read-ushort r)
                flag (lsb/read-ushort r)
                l-seq (lsb/read-int r)
                rnext (lsb/read-int r)
                pnext (inc (lsb/read-int r))
                tlen (lsb/read-int r)
                qname (lsb/read-string r (dec (int l-read-name)))
                _ (lsb/read-bytes r 1)
                cigar (bam/decode-cigar (lsb/read-bytes r (* n-cigar-op 4)))
                seq (bam/decode-seq (lsb/read-bytes r (/ (inc l-seq) 2)) l-seq)
                qual (bam/decode-qual (lsb/read-bytes r (count seq))) ;todo
                rest (lsb/read-bytes r (- block-size
                                          bam/fixed-block-size
                                          (int l-read-name)
                                          (* n-cigar-op 4)
                                          (/ (inc l-seq) 2)
                                          (count seq)))
                options (parse-options rest)]
            (recur (conj alignments (SamAlignment. qname flag rname pos mapq cigar rnext pnext tlen seq qual options)))))))))

(defn slurp-bam
  "Opens a reader on bam-file and reads all its headers and alignments,
  returning a map about sam records."
  [bam-file]
  (with-open [r (DataInputStream. (BlockCompressedInputStream. (file bam-file)))]
    (if-not (Arrays/equals (lsb/read-bytes r 4) (.getBytes bam/bam-magic))
      (throw (IOException. "Invalid BAM file header")))
    (let [header (parse-header (lsb/read-string r (lsb/read-int r)))]
      (let [cnt (lsb/read-int r)]
        (loop [i cnt]
          (if (zero? i)
            nil
            (do (lsb/read-string r (lsb/read-int r))
                (lsb/read-int r)
                (recur (dec i))))))
      (Sam. header (parse-alignments r)))))

(defn- stringify-header [headers]
  (->> (map #(sam/stringify %) headers)
       (str/join \newline)))

(defn- write-tag-value [writer val-type value]
  (condp = val-type
    \A (lsb/write-bytes  writer (char value))
    \i (lsb/write-int    writer (Integer/parseInt value))
    \f (lsb/write-float  writer (Float/parseFloat value))
    \Z (lsb/write-string writer value)
    ;; \H nil
    \B (let [[array-type & array] (str/split value #",")]
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

(defn spit-bam
  "Opposite of slurp-bam. Opens bam-file with writer, writes sam headers and
  alignments, then closes the bam-file."
  [bam-file sam]
  (with-open [w (DataOutputStream. (BlockCompressedOutputStream. bam-file))]
    ;; header
    (lsb/write-bytes w (.getBytes bam/bam-magic)) ; magic
    (let [header (str (stringify-header (:header sam)) \newline)]
      (lsb/write-int w (count header))
      (lsb/write-string w header))
    (lsb/write-int w (count (:header sam)))

    ;; list of reference information
    (doseq [sh (:header sam)]
      (lsb/write-int w (inc (count (:SN (:SQ sh)))))
      (lsb/write-string w (:SN (:SQ sh)))
      (lsb/write-bytes w (byte-array 1 (byte 0)))
      (lsb/write-int w (Integer/parseInt (:LN (:SQ sh)))))

    ;; list of alignments
    (doseq [sa (:alignments sam)]
      (lsb/write-int w (bam/get-block-size sa))

      (lsb/write-int w (bam/get-ref-id sa))

      (lsb/write-int w (bam/get-pos sa))

      (lsb/write-ubyte w (short (inc (count (:qname sa)))))
      (lsb/write-ubyte w (short (:mapq sa)))
      (lsb/write-ushort w (reg->bin (:pos sa) (bam/get-end sa)))

      (lsb/write-ushort w (bam/count-cigar sa))
      (lsb/write-ushort w (:flag sa))

      (lsb/write-int w (bam/get-l-seq sa))

      (lsb/write-int w (bam/get-next-ref-id sa))

      (lsb/write-int w (bam/get-next-pos sa))

      (lsb/write-int w (bam/get-tlen sa))

      (lsb/write-string w (bam/get-read-name sa))
      (lsb/write-bytes w (byte-array 1 (byte 0)))

      (doseq [cigar (bam/get-cigar sa)]
        (lsb/write-int w cigar))

      (lsb/write-bytes w (bam/get-seq sa))

      (lsb/write-bytes w (bam/encode-qual sa))

      ;; options
      (doseq [op (:options sa)]
        (let [[tag value] (first (seq op))]
          (lsb/write-short w (short (bit-or (bit-shift-left (byte (second (name tag))) 8)
                                            (byte (first (name tag))))))
          (lsb/write-bytes w (.getBytes (:type value)))
          (write-tag-value w (first (:type value)) (:value value)))))
    nil))

;;; bai

(defn spit-bai
  "Opposite of slurp. Opens sam/bam-file with writer, writes sam headers and
  alignments, then closes the sam/bam-file."
  [bai-file sam]
  (with-open [w (DataOutputStream. (BlockCompressedOutputStream. bai-file))]
    (lsb/write-bytes w (.getBytes indexer/bai-magic)) ; magic
    ;; TODO
    nil))

;;; fasta, fai

(defn slurp-fasta
  [fa-file]
  (with-open [r (RandomAccessFile. fa-file "r")]
    (loop [fa []
           line (.readLine r)]
      (if (nil? line)
        fa
        (if (= (first line) \>)
          (recur (conj fa {:ref (subs line 1)
                           :offset (.getFilePointer r)
                           :seq (.readLine r)})
                 (.readLine r))
          (recur fa (.readLine r)))))))

(defn spit-fai
  [fai-file fa]
  (with-open [w (writer fai-file)]
    (doseq [ref fa]
      (.write w (:ref ref))
      (.write w \tab)
      (.write w (count (:seq ref)))
      (.write w \tab)
      (.write w (:offset ref))
      ;; todo
      (.newLine w))
    nil))

;;; Automatic file-type detection

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

(ns cljam.io
  (:refer-clojure :exclude [slurp spit])
  (:require [clojure.string :as str]
            [clojure.java.io :refer [reader writer file]]
            (cljam [sam :as sam]
                   [bam :as bam]
                   [lsb :as lsb]
                   [indexer :as indexer]
                   [util :refer [reg->bin string->bytes]]))
  (:import java.util.Arrays
           (java.io DataInputStream DataOutputStream RandomAccessFile IOException)
           (java.nio ByteBuffer ByteOrder)
           (net.sf.samtools.util BlockCompressedInputStream BlockCompressedOutputStream)
           (cljam.sam Sam SamHeader SamAlignment)))

;;; sam

(defn slurp-sam
  "Opens a reader on sam-file and reads all its headers and alignments,
  returning a map about sam records."
  [f]
  (with-open [r (sam/reader f)]
    (Sam. (sam/read-header r)
          (doall (sam/read-alignments r)))))

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

(defn slurp-bam
  "Opens a reader on bam-file and reads all its headers and alignments,
  returning a map about sam records."
  [f]
  (with-open [r (bam/reader f)]
    (Sam. (bam/read-header r)
          (doall (bam/read-alignments r)))))

(defn- stringify-header [headers]
  (str/join \newline
            (map sam/stringify headers)))

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

      (lsb/write-int w (bam/get-ref-id sa (sam/make-refs sam)))

      (lsb/write-int w (bam/get-pos sa))

      (lsb/write-ubyte w (short (inc (count (:qname sa)))))
      (lsb/write-ubyte w (short (:mapq sa)))
      (lsb/write-ushort w (reg->bin (:pos sa) (bam/get-end sa)))

      (lsb/write-ushort w (bam/count-cigar sa))
      (lsb/write-ushort w (:flag sa))

      (lsb/write-int w (bam/get-l-seq sa))

      (lsb/write-int w (bam/get-next-ref-id sa (sam/make-refs sam)))

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
  "Opens a reader on a fasta-file and reads all its contents, returning a map
  about the data."
  [fa-file]
  (with-open [r (RandomAccessFile. fa-file "r")]
    (loop [fa []
           line (.readLine r)]
      (if (nil? line)
        fa
        (if (= (first line) \>)
          (let [ref    (subs line 1)
                offset (.getFilePointer r)
                seq    (.readLine r)
                blen   (count (filter (partial not= \space) seq))]
            (recur (conj fa {:ref ref, :offset offset, :seq seq, :blen blen})
                   (.readLine r)))
          (recur fa (.readLine r)))))))

(defn spit-fai
  "Opens a fai-file with writer, writes fasta index data, then closes the
  fai-file."
  [fai-file fa]
  (with-open [w (writer fai-file)]
    (doseq [ref fa]
      (.write w (:ref ref))
      (.write w "\t")
      (.write w (str (count (:seq ref))))
      (.write w "\t")
      (.write w (str (:offset ref)))
      (.write w "\t")
      (.write w (str (:blen ref)))
      (.write w "\t")
      (.write w (str (inc (count (:seq ref)))))
      (.newLine w))
    nil))

;;; Automatic file-type detection

(defn slurp
  "Opens a reader on sam/bam-file and reads all its headers and alignments,
  returning a map about sam records."
  [f]
  (condp re-find f
      #"\.sam$" (slurp-sam f)
      #"\.bam$" (slurp-bam f)
      (throw (IllegalArgumentException. "Invalid file type"))))

(defn spit
  "Opposite of slurp. Opens sam/bam-file with writer, writes sam headers and
  alignments, then closes the sam/bam-file."
  [f sam]
  (condp re-find f
      #"\.sam$" (spit-sam f sam)
      #"\.bam$" (spit-bam f sam)
      (throw (IllegalArgumentException. "Invalid file type"))))

(ns cljam.io
  (:use [clojure.java.io :only [reader writer]]
        [cljam.util :only [reg-to-bin string-to-bytes]])
  (:require [clojure.string :as str]
            [cljam.sam :as sam]
            [cljam.bam :as bam])
  (:import java.io.DataOutputStream
           (java.nio ByteBuffer ByteOrder)
           net.sf.samtools.util.BlockCompressedOutputStream
           (cljam.sam Sam SamHeader SamAlignment)))

(def byte-buffer (ByteBuffer/allocate 8))
(.order byte-buffer ByteOrder/LITTLE_ENDIAN)

(defn- write-int [writer value]
  (.clear byte-buffer)
  (.putInt byte-buffer value)
  (.write writer (.array byte-buffer) 0 4))

(defn- write-string [writer s]
  (let [data-bytes (string-to-bytes s)]
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
  "Opens a reader on sam-file and reads all its headers and alignments, returning a map about sam records."
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
  "Opposite of slurp-sam. Opens sam-file with writer, writes sam headers and alignments, then closes sam-file."
  [sam-file sam]
  (with-open [w (writer sam-file)]
    (doseq [sh (:header sam)]
      (.write w (sam/stringify sh))
      (.newLine w))
    (doseq [sa (:alignments sam)]
      (.write w (sam/stringify sa))
      (.newLine w))
    nil))

(defn- stringify-header [headers]
  (->> (map #(sam/stringify %) headers)
       (str/join \newline)))

;;; TODO
(defn slurp-bam
  "Opens a reader on bam-file and reads all its headers and alignments, returning a map about sam records."
  [bam-file]
  nil)

(defn spit-bam
  "Opposite of slurp-bam. Opens bam-file with writer, writes bam headers and alignments, then closes bam-file."
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
      (write-ushort w (reg-to-bin (:pos sa) (bam/get-end sa)))

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

      (let [data (bam/get-seq sa)]
        (write-bytes w data))

      (write-bytes w (bam/get-qual sa)))
    nil))

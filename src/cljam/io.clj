(ns cljam.io
  (:require [clojure.string :as str]
            [clojure.java.io :only (file reader) :as io]
            [cljam.sam :as sam])
  (:import (java.io DataOutputStream FileOutputStream)
           net.sf.samtools.util.BlockCompressedOutputStream
           (cljam.sam Sam SamHeader SamAlignment)))

(defn slurp-sam
  "Opens a reader on sam-file and reads all its headers and alignments, returning a map about sam records."
  [sam-file]
  (with-open [r (io/reader sam-file)]
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
  (with-open [w (io/writer sam-file)]
    (doseq [sh (:header sam)]
      (.write w (sam/stringify sh))
      (.newLine w))
    (doseq [sa (:alignments sam)]
      (.write w (sam/stringify sa))
      (.newLine w))
    nil))

(defn- stringify-header [headers]
  (->> (map #(stringify-header-map %) headers)
       (str/join \newline)))

(def fixed-block-size 32)

;;; TODO
(defn slurp-bam
  "Opens a reader on bam-file and reads all its headers and alignments, returning a map about sam records."
  [bam-file]
  nil)

;; (defn spit-bam
;;   "Opposite of slurp-bam. Opens bam-file with writer, writes bam headers and alignments, then closes bam-file."
;;   [bam-file sam]
;;   (with-open [w (DataOutputStream. (BlockCompressedOutputStream. bam-file))]
;;     ;; header
;;     (.write w (.getBytes "BAM\1"))
;;     (.write w (.getBytes (stringify-header (:header sam))))
;;     (.write w (.getBytes "\n"))
;;     ;; alignments
;;     (doseq [alignment-map (:alignments sam)]
;;       (.write w (+ fixed-block-size     ; TODO
;;                    (count (:RNAME alignment-map))
;;                    1
;;                    (* cigar-length 4)
;;                    (/ (+ read-length 1) 2)
;;                    read-length))
;;       (.write w ))
;;     nil))

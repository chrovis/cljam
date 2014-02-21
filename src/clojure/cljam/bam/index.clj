(ns cljam.bam.index
  (:require [clojure.java.io :refer [file]])
  (:import [java.io IOException]
           [cljam.lib.bam SAMSequenceDictionary SAMSequenceRecord BAMFileIndex]))

(deftype BAMIndex [f sequences]
  java.io.Closeable
  (close [this] (.. this f close)))

(defn- make-sequence-dictionary
  [sequences]
  (new SAMSequenceDictionary
       (map (fn [s] (new SAMSequenceRecord (:SN s) (:LN s)))
            sequences)))

;;; FIXME: not bai feature
(defn get-sequence-index
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

(defn bam-index
  [f header]
  (let [bai-f (str f ".bai")]
    (when-not (.exists (file bai-f))
      (throw (IOException. "Could not find BAM Index file")))
    (let [sequences (:SQ header)
          seq-dict (make-sequence-dictionary sequences)
          bai (new BAMFileIndex (file bai-f) seq-dict)]
      (->BAMIndex bai sequences))))

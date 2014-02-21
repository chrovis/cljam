(ns cljam.bam-index
  (:require [cljam.bam-index.core :as bai-core]))

(defn read-index [bai]
  (bai-core/read-index bai))

(defn get-spans
  "Returns regions of a BAM file that may contain an alignment for the given range."
  [bai ref-idx beg end]
  (bai-core/get-spans bai ref-idx beg end))

(defn bam-index
  "Returns a cljam.bam-index.core.BAMIndex."
  [f]
  (bai-core/bam-index f))

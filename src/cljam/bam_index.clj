(ns cljam.bam-index
  "Parser for a BAM index file."
  (:require [cljam.bam-index.core :as bai-core]))

(defn bin-index
  "Returns binning index for the given reference index."
  [bai ref-idx]
  (bai-core/bin-index bai ref-idx))

(defn linear-index
  "Returns linear index for the given reference index."
  [bai ref-idx]
  (bai-core/linear-index bai ref-idx))

(defn get-spans
  "Returns regions of a BAM file that may contain an alignment for the given range."
  [bai ref-idx beg end]
  (bai-core/get-spans bai ref-idx beg end))

(defn bam-index
  "Returns a cljam.bam-index.core.BAMIndex."
  [f]
  (bai-core/bam-index f))

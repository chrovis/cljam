(ns cljam.io.bam-index
  "Parser for a BAM index file."
  (:require [cljam.io.bam-index.core :as bai-core]))

(defn bin-index
  "Returns binning index for the given reference index."
  [f ref-idx]
  (bai-core/bin-index f ref-idx))

(defn linear-index
  "Returns linear index for the given reference index."
  [f ref-idx]
  (bai-core/linear-index f ref-idx))

(defn get-spans
  "Returns regions of a BAM file that may contain an alignment for the given range."
  [bai ref-idx beg end]
  (bai-core/get-spans bai ref-idx beg end))

(defn bam-index
  "Returns a cljam.bam-index.core.BAMIndex."
  [f]
  (bai-core/bam-index f))

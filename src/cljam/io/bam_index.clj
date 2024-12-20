(ns cljam.io.bam-index
  "Parser for a BAM index file."
  (:require
   [cljam.io.bam-index.core :as bai-core]
   [cljam.io.util.bin :as util-bin]))

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
  (util-bin/get-spans bai ref-idx beg end))

(defn get-multi-spans
  "Returns regions of a BAM file that may contain an alignment for the given
  `ranges` which is a sequence of pairs of integers, begin and end."
  [bai ^long ref-idx ranges]
  (util-bin/get-multi-spans bai ref-idx ranges))

(defn get-unplaced-spans
  "Returns a sequence of [start end) pairs of virtual file offsets that may
  contain alignments that don't have RNAME."
  [bai]
  (bai-core/get-unplaced-spans bai))

(defn bam-index
  "Returns a cljam.bam-index.core.BAMIndex."
  [f]
  (bai-core/bam-index f))

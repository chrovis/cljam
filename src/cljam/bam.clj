(ns cljam.bam
  "Read/Write a BAM format file."
  (:require [cljam.bam.core :as bam-core]))

(defn reader
  "Returns BAM file reader of f with options."
  [f & option]
  (bam-core/reader f option))

(defn writer
  "Returns BAM file writer of f."
  [f]
  (bam-core/writer f))

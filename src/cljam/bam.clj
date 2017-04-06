(ns cljam.bam
  "Read/Write a BAM format file."
  (:require [cljam.bam.core :as bam-core]
            [cljam.bam.reader]
            [cljam.bam.writer])
  (:import cljam.bam.reader.BAMReader
           cljam.bam.writer.BAMWriter))

(defn ^BAMReader reader
  "Returns BAM file reader of f with options."
  [f & option]
  (bam-core/reader f option))

(defn ^BAMWriter writer
  "Returns BAM file writer of f."
  [f]
  (bam-core/writer f))

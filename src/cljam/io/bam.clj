(ns cljam.io.bam
  "Read/Write a BAM format file."
  (:require [cljam.io.bam.core :as bam-core]
            [cljam.io.bam.reader]
            [cljam.io.bam.writer])
  (:import cljam.io.bam.reader.BAMReader
           cljam.io.bam.writer.BAMWriter))

(defn ^BAMReader reader
  "Returns BAM file reader of f with options."
  [f & option]
  (bam-core/reader f option))

(defn ^BAMReader clone-reader
  "Clones bam reader sharing persistent objects."
  [r]
  (bam-core/clone-reader r))

(defn ^BAMWriter writer
  "Returns BAM file writer of f."
  [f]
  (bam-core/writer f))

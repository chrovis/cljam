(ns cljam.io.sam
  "Read/Write a SAM format file."
  (:require [cljam.io.sam.reader :as sam-reader]
            [cljam.io.sam.writer :as sam-writer])
  (:import cljam.io.sam.reader.SAMReader
           cljam.io.sam.writer.SAMWriter))

(defn ^SAMReader reader
  "Returns SAM file reader of f."
  [f]
  (sam-reader/reader f))

(defn ^SAMWriter writer
  "Returns SAM file writer of f."
  [f]
  (sam-writer/writer f))

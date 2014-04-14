(ns cljam.sam
  "Read/Write a SAM format file."
  (:refer-clojure :exclude [slurp spit])
  (:require [cljam.io :as io]
            (cljam.sam [reader :as sam-reader]
                       [writer :as sam-writer]))
  (:import cljam.sam.reader.SAMReader
           cljam.sam.writer.SAMWriter))

(defn ^SAMReader reader
  "Returns SAM file reader of f."
  [f]
  (sam-reader/reader f))

(defn ^SAMWriter writer
  "Returns SAM file writer of f."
  [f]
  (sam-writer/writer f))

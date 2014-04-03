(ns cljam.sam
  "Read/Write a SAM format file."
  (:refer-clojure :exclude [slurp spit])
  (:require [cljam.io :as io]
            (cljam.sam [reader :as sam-reader]
                       [writer :as sam-writer])))

(defn reader
  "Returns SAM file reader of f."
  [f]
  (sam-reader/reader f))

(defn writer
  "Returns SAM file writer of f."
  [f]
  (sam-writer/writer f))

(ns cljam.bam
  (:require [cljam.bam.core :as bam-core]))

(defn reader
  [f & option]
  (bam-core/reader f option))

(defn writer
  [f]
  (bam-core/writer f))

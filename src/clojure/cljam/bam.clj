(ns cljam.bam
  (:refer-clojure :exclude [slurp spit])
  (:require [cljam.io :as io]
            (cljam.bam [reader :as bam-reader]
                       [writer :as bam-writer])))

(defn reader
  [f & option]
  (bam-reader/reader f option))

(defn writer
  [f]
  (bam-writer/writer f))

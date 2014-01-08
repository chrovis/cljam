(ns cljam.sam
  (:refer-clojure :exclude [slurp spit])
  (:require [cljam.io :as io]
            (cljam.sam [reader :as sam-reader]
                       [writer :as sam-writer])))

(defn reader
  [f]
  (sam-reader/reader f))

(defn writer
  [f]
  (sam-writer/writer f))

(defn ^:deprecated
  slurp
  "Opens a reader on sam-file and reads all its headers and alignments,
  returning a map about sam records."
  [f]
  (with-open [r (reader f)]
    {:header (io/read-header r)
     :alignments (vec (io/read-alignments r {}))}))

(defn ^:deprecated
  spit
  "Opposite of slurp-sam. Opens sam-file with writer, writes sam headers and
  alignments, then closes the sam-file."
  [f sam]
  (with-open [w (writer f)]
    (io/write-header w (:header sam))
    (io/write-alignments w (:alignments sam) nil)))

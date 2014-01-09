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

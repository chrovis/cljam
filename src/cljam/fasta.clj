(ns cljam.fasta
  "Alpha - subject to change.
  Reader of a FASTA format file."
  (:refer-clojure :exclude [read])
  (:require [cljam.fasta.core :as fa-core]))

(defn reader
  "Returns FASTA file reader of f."
  [f]
  (fa-core/reader f))

(defn read
  [rdr]
  (fa-core/read rdr))

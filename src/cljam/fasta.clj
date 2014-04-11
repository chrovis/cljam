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

(defn read-headers
  "Returns headers as vector. Each element consists of name and offset."
  [rdr]
  (fa-core/read-headers rdr))

(defn read-sequences
  "Reads sequences by line, returning the line-separated sequences
  as lazy sequence."
  [rdr]
  (fa-core/read-sequences rdr))

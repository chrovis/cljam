(ns cljam.twobit
  (:require [cljam.twobit.reader :as tb-reader]
            [cljam.twobit.writer :as tb-writer])
  (:import [cljam.twobit.reader TwoBitReader]
           [cljam.twobit.writer TwoBitWriter]))

;; Reading
;; -------

(defn ^TwoBitReader reader
  "Returns an open cljam.twobit.reader.TwoBitReader of f.
  Should be used inside with-open to ensure the Reader is properly closed."
  [f]
  (tb-reader/reader f))

(defn ^String read-sequence
  "Reads sequence at the given region from reader.
   Pass {:mask? true} to enable masking of sequence."
  ([rdr region]
   (read-sequence rdr region {}))
  ([rdr region option]
   (tb-reader/read-sequence rdr region option)))

(defn read-all-sequences
  "Reads all sequences in file and returns as a sequence of maps.
   Pass {:mask? true} to enable masking of sequence."
  ([rdr]
   (read-all-sequences rdr {}))
  ([rdr option]
   (tb-reader/read-all-sequences rdr option)))

;; Writing
;; -------

(defn ^TwoBitWriter writer
  "Returns an open cljam.twobit.writer.TwoBitWriter of f.
  Should be used inside with-open to ensure the Writer is properly closed."
  [f]
  (tb-writer/writer f))

(defn write-sequences
  "Write sequences to writer.
  Input seqs must be a sequence of maps containing {:name , :sequence}."
  [wtr seqs]
  (tb-writer/write-sequences wtr seqs))

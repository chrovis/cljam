(ns cljam.fasta-index.writer
  "Writing features for a FASTA index file."
  (:require [clojure.string :as str])
  (:import [java.io BufferedWriter]))

;; FAIWriter
;; ---------

(deftype FAIWriter [writer f]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.writer this))))

;; Indexing
;; --------

(defn write-sq!
  [^BufferedWriter wtr sq]
  (.write wtr (str/join "\t"
                        [(:rname sq)
                         (:len sq)
                         (:offset sq)
                         (:line-blen sq)
                         (:line-len sq)]))
  (.newLine wtr))

(defn- write-index*!
  [^BufferedWriter wtr seqs]
  (doseq [sq seqs]
    (write-sq! wtr sq)))

(defn write-index!
  [^FAIWriter wtr seqs]
  (write-index*! (.writer wtr) seqs))

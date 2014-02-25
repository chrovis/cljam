(ns cljam.fasta-index.writer
  "Writing features for a FASTA index file."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io BufferedWriter Closeable]))

(deftype FAIWriter [f writer]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this))))

(defn write-sq!
  [^BufferedWriter wtr sq]
  (.write wtr (str/join "\t"
                        [(:rname sq)
                         (count (:seq sq))
                         (:offset sq)
                         (:blen sq)
                         (inc (count (:seq sq)))]))
  (.newLine wtr))

(defn- write-index*!
  [^BufferedWriter wtr seqs]
  (doseq [sq seqs]
    (write-sq! wtr sq)))

(defn write-index!
  [^FAIWriter wtr seqs]
  (write-index*! (.writer wtr) seqs))

(defn writer
  [f]
  (->FAIWriter f (io/writer f)))

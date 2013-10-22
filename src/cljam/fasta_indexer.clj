(ns cljam.fasta-indexer
  (:refer-clojure :exclude [spit])
  (:require [clojure.java.io :refer [writer]]
            [clojure.string :refer [join]]
            [cljam.fasta :as fasta])
  (:import [java.io BufferedWriter RandomAccessFile]))

(defn write-sq
  [^BufferedWriter wrtr sq]
  (.write wrtr (join "\t"
                     [(:ref sq)
                      (count (:seq sq))
                      (:offset sq)
                      (:blen sq)
                      (inc (count (:seq sq)))]))
  (.newLine wrtr))

(defn spit
  "Opens a FAI file with writer, writes fasta index data, then closes the file."
  [fai fa-sq]
  (with-open [w ^BufferedWriter (writer fai)]
    (doseq [sq fa-sq]
      (write-sq w sq))))

(defn create-index
  "Create a FAI file from the specified fasta file."
  [fasta out-fai]
  (with-open [r ^RandomAccessFile (fasta/reader fasta)
              w ^BufferedWriter (writer out-fai)]
    (doseq [sq (fasta/read r)]
      (write-sq w sq))))

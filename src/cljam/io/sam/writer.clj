(ns cljam.io.sam.writer
  "Provides writing features."
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.io.sam.util :refer [stringify-header
                                       stringify-alignment]]
            [cljam.io.protocols :as protocols])
  (:import [java.io BufferedWriter Closeable]))

(declare write-header* write-alignments* write-blocks*)

;; SAMWriter
;; ---------

(deftype SAMWriter [^BufferedWriter writer f]
  Closeable
  (close [this]
    (.close writer))
  protocols/IWriter
  (writer-path [this]
    (.f this))
  protocols/IAlignmentWriter
  (write-header [this header]
    (write-header* this header))
  (write-refs [this refs]
    (logging/debug "SAMWriter does not support write-refs"))
  (write-alignments [this alignments refs]
    (write-alignments* this alignments refs))
  (write-blocks [this blocks]
    (write-blocks* this blocks)))

;; Writing
;; -------

(defn- write-header*
  [^SAMWriter sam-writer header]
  (let [wtr ^BufferedWriter (.writer sam-writer)]
    (.write wtr ^String (stringify-header header))
    (.newLine wtr)))

(defn- write-alignments*
  [^SAMWriter sam-writer alns _]
  (let [wtr ^BufferedWriter (.writer sam-writer)]
   (doseq [a alns]
     (.write wtr ^String (stringify-alignment a))
     (.newLine wtr))))

(defn- write-blocks*
  [^SAMWriter sam-writer blocks]
  (let [wtr ^BufferedWriter (.writer sam-writer)]
   (doseq [b blocks]
     (.write wtr ^String (:data b))
     (.newLine wtr))))

;; Public
;; ------

(defn ^SAMWriter writer
  [f]
  (->SAMWriter (cio/writer f)
               (.getAbsolutePath (cio/file f))))

(defn write-blocks-rf
  "Returns a reducing function which writes SAM blocks to the given writer."
  [^SAMWriter sam-writer header]
  (let [wtr ^BufferedWriter (.writer sam-writer)]
    (fn write-blocks-rf-rf
      ([]
       (write-header* sam-writer header))
      ([_])
      ([_ input]
       (.write wtr ^String (:data input))
       (.newLine wtr)))))

(defn write-alignments-rf
  "Returns a reducing function which writes alignments to the given SAM writer."
  [sam-writer header]
  ((map (comp (partial array-map :data) stringify-alignment))
   (write-blocks-rf sam-writer header)))

(defn write-blocks-xf
  "Returns a transducer which writes SAM blocks to the given writer as
  side-effects. Note that this function immediately writes header and reference
  info when invoked to prevent them being written multiple times."
  [^SAMWriter sam-writer header]
  (let [wtr ^BufferedWriter (.writer sam-writer)]
    (write-header* sam-writer header)
    (fn write-blocks-xf [rf]
      (fn write-blocks-xf-xf-rf
        ([]
         (rf))
        ([result]
         (rf result))
        ([result input]
         (.write wtr ^String (:data input))
         (.newLine wtr)
         (rf result input))))))

(defn write-alignments-xf
  "Returns a stateful transducer which writes alignments to the given SAM
  writer as side-effects. Note that this function immediately writes
  header and reference info when invoked to prevent them being written multiple
  times."
  [sam-writer header]
  (comp
   (map (comp (partial array-map :data) stringify-alignment))
   (write-blocks-xf sam-writer header)))

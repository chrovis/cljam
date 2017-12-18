(ns cljam.io.sam.writer
  "Provides writing features."
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.io.sam.util :refer [stringify-header
                                       stringify-alignment]]
            [cljam.io.protocols :as protocols])
  (:import [java.io BufferedWriter Closeable]))

(declare write-alignments* write-blocks*)

;; SAMWriter
;; ---------

(deftype SAMWriter [^BufferedWriter writer f header]
  Closeable
  (close [this]
    (.close writer))
  protocols/IWriter
  (writer-path [this]
    (.f this))
  protocols/IAlignmentWriter
  (write-alignments [this alignments]
    (write-alignments* this alignments))
  (write-blocks [this blocks]
    (write-blocks* this blocks)))

;; Writing
;; -------

(defn- write-header
  [^SAMWriter sam-writer]
  (let [wtr ^BufferedWriter (.writer sam-writer)
        header (.header sam-writer)]
    (.write wtr ^String (stringify-header header))
    (.newLine wtr)))

(defn- write-alignments*
  [^SAMWriter sam-writer alns]
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
  [f header]
  (doto (->SAMWriter (cio/writer f) (.getAbsolutePath (cio/file f)) header)
    (write-header)))

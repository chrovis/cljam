(ns cljam.io.sam.writer
  "Provides writing features."
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.io.sam.util :as sam-util]
            [cljam.io.sam.util.header :as header]
            [cljam.io.protocols :as protocols]
            [cljam.util :as util])
  (:import [java.io BufferedWriter Closeable]))

(declare write-header* write-alignments* write-blocks*)

;; SAMWriter
;; ---------

(deftype SAMWriter [^BufferedWriter writer url]
  Closeable
  (close [this]
    (.close writer))
  protocols/IWriter
  (writer-url [this]
    (.url this))
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
    (.write wtr ^String (header/stringify-header header))
    (.newLine wtr)))

(defn- write-alignments*
  [^SAMWriter sam-writer alns _]
  (let [wtr ^BufferedWriter (.writer sam-writer)]
   (doseq [a alns]
     (.write wtr ^String (sam-util/stringify-alignment a))
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
               (util/as-url f)))

(ns cljam.sam.writer
  "Provides writing features."
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as logging]
            [cljam.util.sam-util :refer [stringify-header
                                         stringify-alignment]]
            [cljam.io])
  (:import java.io.BufferedWriter))

(declare write-header* write-alignments* write-blocks*)

;; SAMWriter
;; ---------

(deftype SAMWriter [^java.io.BufferedWriter writer f]
  java.io.Closeable
  (close [this]
    (.close writer))
  cljam.io/ISAMWriter
  (writer-path [this]
    (.f this))
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
  [^SAMWriter sam-writer alns refs]
  (let [wtr ^BufferedWriter (.writer sam-writer)]
   (doseq [a alns]
     (.write wtr ^String (stringify-alignment a))
     (.newLine wtr))))

(defn- write-blocks*
  [^SAMWriter sam-writer blocks]
  (let [wtr ^BufferedWriter (.writer sam-writer)]
   (doseq [b blocks]
     (.write wtr ^String (:line b))
     (.newLine wtr))))

;; Public
;; ------

(defn ^SAMWriter writer
  [f]
  (->SAMWriter (clojure.java.io/writer f)
               (.getAbsolutePath (io/file f))))

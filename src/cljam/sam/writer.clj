(ns cljam.sam.writer
  "Provides writing features."
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as logging]
            [cljam.util.sam-util :refer [stringify-header
                                         stringify-alignment]]
            [cljam.io])
  (:import java.io.BufferedWriter))

;; SAMWriter
;; ---------

(deftype SAMWriter [^java.io.BufferedWriter writer f]
  java.io.Closeable
  (close [this]
    (.close writer)))

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

;; Public
;; ------

(defn ^SAMWriter writer
  [f]
  (->SAMWriter (clojure.java.io/writer f)
               (.getAbsolutePath (io/file f))))

(extend-type SAMWriter
  cljam.io/ISAMWriter
  (writer-path [this]
    (.f this))
  (write-header [this header]
    (write-header* this header))
  (write-refs [this refs]
    (logging/info "SAMWriter does not support write-refs"))
  (write-alignments [this alignments refs]
    (write-alignments* this alignments refs))
  (write-blocks [this blocks]
    ;;(logging/info "SAMWriter does not support write-blocks")
    )
  (write-coordinate-blocks [this blocks]
    ;;(logging/info "SAMWriter does not support write-coordinate-blocks")
    ))

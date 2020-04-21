(ns cljam.io.dict.core
  "The core of dictionary features."
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.io.dict.writer :as writer]
            [cljam.util :as util])
  (:import cljam.io.dict.writer.DICTWriter))

;; Reading
;; -------

;; TODO

;; Writing
;; -------

(defn ^DICTWriter writer
  "Opens f, returning a `cljam.dict.writer.DICTWriter`. Should be used inside
  `with-open` to ensure the writer is properly closed."
  [f]
  (DICTWriter. (cio/writer f)
               (util/as-url f)))

(defn create-dict
  "Creates a FASTA sequence dictionary file (.dict) from the specified FASTA
  file. The unfinished file will be deleted when failing."
  [f headers sequences ur]
  (with-open [w (writer f)]
    (try
      (writer/write-dict! w headers sequences ur)
      (catch Exception e
        (cio/delete-file (.url w))
        (logging/error "Failed to create dictionary")
        (throw e)))))

(ns cljam.dict.core
  "The core of dictionary features."
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as logging]
            [me.raynes.fs :as fs]
            [cljam.dict.writer :as writer])
  (:import cljam.dict.writer.DICTWriter))

;; Reading
;; -------

;; TODO

;; Writing
;; -------

(defn ^DICTWriter writer
  "Opens f, returning a `cljam.dict.writer.DICTWriter`. Should be used inside
  `with-open` to ensure the writer is properly closed."
  [f]
  (DICTWriter. (io/writer f)
               (.getAbsolutePath (io/file f))))

(defn create-dict
  "Creates a FASTA sequence dictionary file (.dict) from the specified FASTA
  file. The unfinished file will be deleted when failing."
  [f reads ur]
  (with-open [w (writer f)]
    (try
      (writer/write-dict! w reads ur)
      (catch Exception e (do
                           (fs/delete (.f w))
                           (logging/error "Failed to create dictionary")
                           (throw e))))))

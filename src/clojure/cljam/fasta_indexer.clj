(ns cljam.fasta-indexer
  (:refer-clojure :exclude [spit])
  (:require [clojure.java.io :refer [writer]]))

(defn spit
  "Opens a fai-file with writer, writes fasta index data, then closes the
  fai-file."
  [fai-file fa]
  (with-open [w (writer fai-file)]
    (doseq [ref fa]
      (.write w (:ref ref))
      (.write w "\t")
      (.write w (str (count (:seq ref))))
      (.write w "\t")
      (.write w (str (:offset ref)))
      (.write w "\t")
      (.write w (str (:blen ref)))
      (.write w "\t")
      (.write w (str (inc (count (:seq ref)))))
      (.newLine w))
    nil))

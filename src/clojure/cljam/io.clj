(ns cljam.io)

;;; Protocol

(defprotocol ISAMReader
  (read-header [this])
  (read-refs [this])
  (read-alignments [this option]))

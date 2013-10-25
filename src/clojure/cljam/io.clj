(ns cljam.io)

;;; Protocol

(defprotocol ISAMReader
  (read-header [this])
  (read-refs [this])
  (read-alignments [this option])
  (read-blocks [this])
  (read-coordinate-blocks [this]))

(defprotocol ISAMWriter
  (write-header [this header])
  (write-refs [this header])
  (write-alignments [this alignments header])
  (write-blocks [this blocks])
  (write-coordinate-blocks [this blocks]))

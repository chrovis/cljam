(ns cljam.io)

;;; Protocol

(defprotocol ISAMReader
  (read-header [this])
  (read-refs [this])
  (read-alignments [this option]))

(defprotocol ISAMWriter
  (write-header [this header])
  (write-refs [this refs])
  (write-alignments [this alignments refs]))

(ns cljam.protocol)

(defprotocol ISAMReader
  (read-header [this])
  (read-refs [this]))

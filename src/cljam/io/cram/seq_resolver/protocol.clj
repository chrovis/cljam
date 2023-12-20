(ns cljam.io.cram.seq-resolver.protocol)

(defprotocol ISeqResolver
  (resolve-sequence [this region]))

(extend-protocol ISeqResolver
  nil
  (resolve-sequence [_ region]
    (throw
     (ex-info "reference was not specified, but tried to resolve sequence"
              region))))

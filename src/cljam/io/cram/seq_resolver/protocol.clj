(ns cljam.io.cram.seq-resolver.protocol)

(defprotocol ISeqResolver
  (resolve-sequence [this chr start end]))

(extend-protocol ISeqResolver
  nil
  (resolve-sequence [_ chr start end]
    (throw
     (ex-info "reference was not specified, but tried to resolve sequence"
              {:chr chr :start start :end end}))))

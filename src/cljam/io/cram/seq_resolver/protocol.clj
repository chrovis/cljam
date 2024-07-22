(ns cljam.io.cram.seq-resolver.protocol)

(defprotocol ISeqResolver
  (resolve-sequence [this chr] [this chr start end]))

(extend-protocol ISeqResolver
  nil
  (resolve-sequence [this chr]
    (resolve-sequence this chr nil nil))
  (resolve-sequence [_ chr start end]
    (throw
     (ex-info "reference was not specified, but tried to resolve sequence"
              {:chr chr :start start :end end}))))

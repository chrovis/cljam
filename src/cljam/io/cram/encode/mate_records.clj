(ns cljam.io.cram.encode.mate-records
  (:require [cljam.io.sam.util.flag :as util.flag])
  (:import [java.util HashMap]))

(defprotocol IMateResolver
  (resolve-mate! [this i record]))

(defn make-mate-resolver
  "Creates a new mate resolver."
  []
  (let [record-indices (HashMap.)]
    (reify IMateResolver
      (resolve-mate! [_ i {:keys [flag qname]}]
        (when (= (bit-and (long flag)
                          (util.flag/encoded #{:multiple :secondary :supplementary}))
                 (util.flag/encoded #{:multiple}))
          (if-let [mate-index (.get record-indices qname)]
            (do (.remove record-indices mate-index)
                mate-index)
            (.put record-indices qname i)))))))

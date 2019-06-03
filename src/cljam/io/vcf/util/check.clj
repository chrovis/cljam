(ns cljam.io.vcf.util.check
  (:require [cljam.io.sequence :as io-seq]))

(defn same-ref?
  "Checks if a REF allele matches the reference sequence."
  [seq-reader {:keys [chr ^long pos ^String ref]}]
  (let [ref-len (.length ref)
        ref-region {:chr chr :start pos :end (dec (+ pos ref-len))}
        ref-seq (io-seq/read-sequence seq-reader ref-region {:mask? false})]
    (and ref-seq (.equalsIgnoreCase ^String ref-seq ref))))

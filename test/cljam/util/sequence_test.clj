(ns cljam.util.sequence-test
  (:require [clojure.test :refer :all]
            [cljam.util.sequence :as util-seq]))

(deftest revcomp
  (are [?forward ?reverse]
      (and (= (util-seq/revcomp ?forward) ?reverse)
           (= (util-seq/revcomp ?reverse) ?forward))
    "" ""
    "A" "T"
    "G" "C"
    "N" "N"
    "a" "t"
    "g" "c"
    "n" "n"
    "AT" "AT"
    "AAT" "ATT"
    "ACGTGT" "ACACGT"
    "GAANTGGN" "NCCANTTC"
    "aacacacnnnnacacTTTAGAGCNNacacnttg" "caangtgtNNGCTCTAAAgtgtnnnngtgtgtt"))

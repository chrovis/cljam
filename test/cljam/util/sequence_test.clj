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

(deftest atgcn?
  (are [?seq ?expected]
      (= ?expected (util-seq/atgcn? ?seq))
    "" true
    "A" true
    "t" true
    "n" true
    "N" true
    "U" false
    "*" false
    "AT" true
    "At" true
    "AtN" true
    "ACM" false
    "AoT" false
    "aacacacnnnnacacTTTAGAGCNNacacnttg" true))

(deftest ->atgcn
  (are [?seq ?expected]
      (= ?expected (util-seq/->atgcn ?seq))
    nil nil
    "" ""
    "A" "A"
    "a" "a"
    "N" "N"
    "n" "n"
    "U" "N"
    "u" "N"
    "ATGCNatgcn" "ATGCNatgcn"
    "*AT>gc" "NATNgc"))

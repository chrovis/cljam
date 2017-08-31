(ns cljam.util.t-chromosome
  (:require [clojure.test :refer :all]
            [cljam.util.chromosome :as chr]))

(deftest normalize-chromosome-key
  (are [?key ?normalized-key] (= (chr/normalize-chromosome-key ?key)
                                 ?normalized-key)
    "chr1" "chr1"
    "chr1" "chr1"
    "chr01" "chr1"
    "chr22" "chr22"
    "chrX" "chrX"
    "chrY" "chrY"
    "chrM" "chrM"
    "chrMT" "chrMT"
    "chrUn" "chrUn"

    "Chr1" "chr1"
    "Chr01" "chr1"
    "Chr22" "chr22"
    "ChrX" "chrX"
    "ChrY" "chrY"
    "ChrM" "chrM"
    "ChrMT" "chrMT"
    "ChrUn" "chrUn"

    "CHR1" "chr1"
    "CHR01" "chr1"
    "CHR22" "chr22"
    "CHRX" "chrX"
    "CHRY" "chrY"
    "CHRM" "chrM"
    "CHRMT" "chrMT"
    "CHRUN" "chrUn"

    "1" "chr1"
    "01" "chr1"
    "22" "chr22"
    "X" "chrX"
    "Y" "chrY"
    "M" "chrM"
    "MT" "chrMT"
    "Un" "chrUn"

    "x" "chrX"
    "y" "chrY"
    "m" "chrM"
    "mt" "chrMT"
    "un" "chrUn"

    "_1" "_1"
    "GL000226_1" "GL000226_1"
    "GL000207.1" "GL000207_1"
    "NC_007605" "NC_007605"
    "hs37d5" "hs37d5"

    "chr4_GL000257v2_alt" "chr4_GL000257v2_alt"
    "4_GL000257v2_alt" "chr4_GL000257v2_alt"
    "chr4_GL000257V2_ALT" "chr4_GL000257v2_alt"

    "chr14_KI270723v1_random" "chr14_KI270723v1_random"
    "14_KI270723v1_random" "chr14_KI270723v1_random"
    "14_KI270723V1_Random" "chr14_KI270723v1_random"

    "chrUn_KI270316v1" "chrUn_KI270316v1"
    "Un_KI270316v1" "chrUn_KI270316v1"
    "Un_KI270316V1" "chrUn_KI270316v1"

    ;; TODO: Add more SN name
    )

  ;; test for cascaded normalization
  (is (= (-> "17_KI270730V1_RANDOM" chr/normalize-chromosome-key)
         (-> "17_KI270730V1_RANDOM" chr/normalize-chromosome-key chr/normalize-chromosome-key)
         (-> "17_KI270730V1_RANDOM" chr/normalize-chromosome-key chr/normalize-chromosome-key chr/normalize-chromosome-key)))

  (are [?key ?trimmed-key] (= (chr/trim-chromosome-key ?key) ?trimmed-key)
    "chr1" "1"
    "Chr2" "2"
    "CHR3" "3"
    "4" "4"
    "X" "X"

    ;; TODO: Add more SN name
    ))

(def split-version-suffix #'chr/split-version-suffix)

(deftest split-version-suffix-test
  (are [?key ?base ?version-suffix] (= (split-version-suffix ?key) [?base ?version-suffix])
    "chr4_GL000257v2_alt" "chr4_GL000257" "v2_alt"
    "chr4_GL000257V2_ALT" "chr4_GL000257" "V2_ALT"

    "chr14_KI270723v1_random" "chr14_KI270723" "v1_random"
    "chr14_KI270723V1_Random" "chr14_KI270723" "V1_Random"))

(deftest is-primary-chromosome?-test
  (are [?key ?pattern] (= (chr/is-primary-chromosome? ?key) ?pattern)
    "chr1" true
    "chr22" true

    "1" true
    "X" true

    "Un" false
    "chrUn_KI270316v1" false

    "chr4_GL000257v2_alt" false
    "14_KI270723V1_random" false))

(deftest chromosome-order-key
  (are [?in ?out] (= (sort-by chr/chromosome-order-key ?in) ?out)
    ["chr2" "chr11" "chr3" "chr1" "chr21"] ["chr1" "chr2" "chr3" "chr11" "chr21"]
    ["2" "11" "3" "1" "21"] ["1" "2" "3" "11" "21"]
    ["chrM_foo" "chrMT_foo"] ["chrM_foo" "chrMT_foo"]

    ["chrY" "chrM_foo" "chrMT" "chrM" "chr22" "chrUn" "chrMT_foo" "chrX"]
    ["chr22" "chrX" "chrY" "chrM" "chrM_foo" "chrMT" "chrMT_foo" "chrUn"]

    ["Y" "M_foo" "MT" "M" "22" "Z" "Un" "MT_foo" "X"]
    ["22" "X" "Y" "M" "M_foo" "MT" "MT_foo" "Un" "Z"]

    ["chr19_KI270930v1_alt" "chr22_KI270879v1_alt" "chrUn_KI270425v1" "chrX" "chrUn_KI270423v1" "chrUn_KI270590v1"
     "chr1" "chr9_GL383541v1_alt" "chr10_GL383545v1_alt" "chr6_GL000253v2_alt" "chrY" "chr9_KI270717v1_random"
     "chr22_KI270734v1_random" "chr2" "chr18_GL383572v1_alt" "chr6_GL000251v2_alt" "chr9_KI270823v1_alt"
     "chr19_KI270916v1_alt" "chr22_KI270875v1_alt" "chrUn_KI270330v1" "chr19_GL949753v2_alt" "chr11"
     "chrUn_KI270312v1" "chr4_GL000008v2_random" "chr19_KI270929v1_alt"]
    ["chr1" "chr2" "chr4_GL000008v2_random" "chr6_GL000251v2_alt" "chr6_GL000253v2_alt"
     "chr9_GL383541v1_alt" "chr9_KI270717v1_random" "chr9_KI270823v1_alt" "chr10_GL383545v1_alt" "chr11"
     "chr18_GL383572v1_alt" "chr19_GL949753v2_alt" "chr19_KI270916v1_alt" "chr19_KI270929v1_alt"
     "chr19_KI270930v1_alt" "chr22_KI270734v1_random" "chr22_KI270875v1_alt" "chr22_KI270879v1_alt" "chrX" "chrY"
     "chrUn_KI270312v1" "chrUn_KI270330v1" "chrUn_KI270423v1" "chrUn_KI270425v1" "chrUn_KI270590v1"]))

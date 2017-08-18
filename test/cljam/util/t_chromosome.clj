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

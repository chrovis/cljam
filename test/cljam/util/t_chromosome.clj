(ns cljam.util.t-chromosome
  (:require [clojure.test :refer :all]
            [cljam.util.chromosome :as chr]))

(deftest normalize-chromosome-key
  (is (= (chr/normalize-chromosome-key "chr1") "chr1"))
  (is (= (chr/normalize-chromosome-key "chr01") "chr1"))
  (is (= (chr/normalize-chromosome-key "chr22") "chr22"))
  (is (= (chr/normalize-chromosome-key "chrX") "chrX"))
  (is (= (chr/normalize-chromosome-key "chrY") "chrY"))
  (is (= (chr/normalize-chromosome-key "chrM") "chrM"))
  (is (= (chr/normalize-chromosome-key "chrMT") "chrMT"))

  (is (= (chr/normalize-chromosome-key "Chr1") "chr1"))
  (is (= (chr/normalize-chromosome-key "Chr01") "chr1"))
  (is (= (chr/normalize-chromosome-key "Chr22") "chr22"))
  (is (= (chr/normalize-chromosome-key "ChrX") "chrX"))
  (is (= (chr/normalize-chromosome-key "ChrY") "chrY"))
  (is (= (chr/normalize-chromosome-key "ChrM") "chrM"))
  (is (= (chr/normalize-chromosome-key "ChrMT") "chrMT"))

  (is (= (chr/normalize-chromosome-key "CHR1") "chr1"))
  (is (= (chr/normalize-chromosome-key "CHR01") "chr1"))
  (is (= (chr/normalize-chromosome-key "CHR22") "chr22"))
  (is (= (chr/normalize-chromosome-key "CHRX") "chrX"))
  (is (= (chr/normalize-chromosome-key "CHRY") "chrY"))
  (is (= (chr/normalize-chromosome-key "CHRM") "chrM"))
  (is (= (chr/normalize-chromosome-key "CHRMT") "chrMT"))

  (is (= (chr/normalize-chromosome-key "1") "chr1"))
  (is (= (chr/normalize-chromosome-key "01") "chr1"))
  (is (= (chr/normalize-chromosome-key "22") "chr22"))
  (is (= (chr/normalize-chromosome-key "X") "chrX"))
  (is (= (chr/normalize-chromosome-key "Y") "chrY"))
  (is (= (chr/normalize-chromosome-key "M") "chrM"))
  (is (= (chr/normalize-chromosome-key "MT") "chrMT"))

  (is (= (chr/normalize-chromosome-key "x") "chrX"))
  (is (= (chr/normalize-chromosome-key "y") "chrY"))
  (is (= (chr/normalize-chromosome-key "m") "chrM"))
  (is (= (chr/normalize-chromosome-key "mt") "chrMT"))

  (is (= (chr/normalize-chromosome-key "_1") "_1"))
  (is (= (chr/normalize-chromosome-key "GL000226_1") "GL000226_1"))
  (is (= (chr/normalize-chromosome-key "GL000207.1") "GL000207_1"))
  (is (= (chr/normalize-chromosome-key "NC_007605") "NC_007605"))
  (is (= (chr/normalize-chromosome-key "hs37d5") "hs37d5"))

  ;; TODO: Add more SN name
  )

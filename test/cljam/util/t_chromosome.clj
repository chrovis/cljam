(ns cljam.util.t-chromosome
  (:use midje.sweet)
  (:require [cljam.util.chromosome :as chr]))

(fact "about normalize-chromosome-key"
  (chr/normalize-chromosome-key "chr1") => "chr1"
  (chr/normalize-chromosome-key "chr01") => "chr1"
  (chr/normalize-chromosome-key "chr22") => "chr22"
  (chr/normalize-chromosome-key "chrX") => "chrX"
  (chr/normalize-chromosome-key "chrY") => "chrY"
  (chr/normalize-chromosome-key "chrM") => "chrM"
  (chr/normalize-chromosome-key "chrMT") => "chrMT"

  (chr/normalize-chromosome-key "Chr1") => "chr1"
  (chr/normalize-chromosome-key "Chr01") => "chr1"
  (chr/normalize-chromosome-key "Chr22") => "chr22"
  (chr/normalize-chromosome-key "ChrX") => "chrX"
  (chr/normalize-chromosome-key "ChrY") => "chrY"
  (chr/normalize-chromosome-key "ChrM") => "chrM"
  (chr/normalize-chromosome-key "ChrMT") => "chrMT"

  (chr/normalize-chromosome-key "CHR1") => "chr1"
  (chr/normalize-chromosome-key "CHR01") => "chr1"
  (chr/normalize-chromosome-key "CHR22") => "chr22"
  (chr/normalize-chromosome-key "CHRX") => "chrX"
  (chr/normalize-chromosome-key "CHRY") => "chrY"
  (chr/normalize-chromosome-key "CHRM") => "chrM"
  (chr/normalize-chromosome-key "CHRMT") => "chrMT"

  (chr/normalize-chromosome-key "1") => "chr1"
  (chr/normalize-chromosome-key "01") => "chr1"
  (chr/normalize-chromosome-key "22") => "chr22"
  (chr/normalize-chromosome-key "X") => "chrX"
  (chr/normalize-chromosome-key "Y") => "chrY"
  (chr/normalize-chromosome-key "M") => "chrM"
  (chr/normalize-chromosome-key "MT") => "chrMT"

  (chr/normalize-chromosome-key "x") => "chrX"
  (chr/normalize-chromosome-key "y") => "chrY"
  (chr/normalize-chromosome-key "m") => "chrM"
  (chr/normalize-chromosome-key "mt") => "chrMT"

  (chr/normalize-chromosome-key "_1") => "_1"
  (chr/normalize-chromosome-key "GL000226_1") => "GL000226_1"
  (chr/normalize-chromosome-key "GL000207.1") => "GL000207_1"
  (chr/normalize-chromosome-key "NC_007605") => "NC_007605"
  (chr/normalize-chromosome-key "hs37d5") => "hs37d5"

  ;; TODO: Add more SN name
  )

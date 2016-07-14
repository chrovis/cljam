(ns cljam.vcf.t-writer
  (:require [midje.sweet :refer :all]
            [midje.util :refer [testable-privates]]
            [cljam.t-common :refer :all]
            [cljam.vcf.writer :refer :all]))

(testable-privates cljam.vcf.writer
                   stringify-meta-info-pedigree
                   stringify-data-line-alt
                   stringify-data-line-qual)

(fact "about stringify-meta-info-pedigree"
  (stringify-meta-info-pedigree {:name-0 "G0-ID"
                                 :name-1 "G1-ID"}) => "Name_0=G0-ID,Name_1=G1-ID"
  (stringify-meta-info-pedigree {:name-1 "G1-ID"
                                 :name-0 "G0-ID"}) => "Name_0=G0-ID,Name_1=G1-ID")

(fact "about stringify-data-line-alt"
  (stringify-data-line-alt ["C"]) => "C"
  (stringify-data-line-alt ["C" "G"]) => "C,G"
  (stringify-data-line-alt nil) => nil)

(fact "about stringify-data-line-qual"
  (stringify-data-line-qual 10.0) => "10"
  (stringify-data-line-qual 9.6) => "9.6"
  (stringify-data-line-qual nil) => nil)

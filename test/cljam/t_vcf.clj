(ns cljam.t-vcf
  (:require [midje.sweet :refer :all]
            [cljam.t-common :refer :all]
            [cljam.vcf :as vcf]))

(fact "`meta-info` returns meta-information of the VCF as a map"
  (with-open [rdr (vcf/reader test-vcf-file)]
    (vcf/meta-info rdr) => test-vcf-meta-info))

(fact "`header` returns header line of the VCF as a vector"
  (with-open [rdr (vcf/reader test-vcf-file)]
    (vcf/header rdr) => test-vcf-header))

(fact "`read-variants` returns data lines of the VCF as a lazy sequence"
  (with-open [rdr (vcf/reader test-vcf-file)]
    (vcf/read-variants rdr) => test-vcf-variants))

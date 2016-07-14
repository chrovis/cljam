(ns cljam.t-vcf
  (:require [midje.sweet :refer :all]
            [cljam.t-common :refer :all]
            [cljam.vcf :as vcf]))

(def ^:private temp-file (str temp-dir "/test.vcf"))

(defn- slurp-vcf-for-test
  [f]
  (with-open [rdr (vcf/reader f)]
    {:meta-info (vcf/meta-info rdr)
     :header (vcf/header rdr)
     :variants (doall (vcf/read-variants rdr))}))

(defn- spit-vcf-for-test
  [f meta-info header variants]
  (with-open [wtr (vcf/writer f meta-info header)]
    (vcf/write-variants wtr variants)))

(fact "`meta-info` returns meta-information of the VCF as a map"
  (with-open [rdr (vcf/reader test-vcf-file)]
    (vcf/meta-info rdr) => test-vcf-meta-info))

(fact "`header` returns header line of the VCF as a vector"
  (with-open [rdr (vcf/reader test-vcf-file)]
    (vcf/header rdr) => test-vcf-header))

(fact "`read-variants` returns data lines of the VCF as a lazy sequence"
  (with-open [rdr (vcf/reader test-vcf-file)]
    (vcf/read-variants rdr) => test-vcf-variants))

(with-state-changes [(before :facts (prepare-cache!))
                     (after :facts (clean-cache!))]
  (fact "about writing VCF"
    (spit-vcf-for-test temp-file test-vcf-meta-info test-vcf-header
                       test-vcf-variants) => anything
    (slurp-vcf-for-test temp-file) => {:meta-info test-vcf-meta-info
                                       :header test-vcf-header
                                       :variants test-vcf-variants}))

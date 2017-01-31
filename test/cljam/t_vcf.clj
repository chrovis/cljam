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

(fact "`meta-info` returns meta-information of the VCF (v4.0) as a map"
  (with-open [rdr (vcf/reader test-vcf-v4_0-file)]
    (vcf/meta-info rdr) => test-vcf-v4_0-meta-info))

(fact "`meta-info` returns meta-information of the VCF (v4.3) as a map"
  (with-open [rdr (vcf/reader test-vcf-v4_3-file)]
    (vcf/meta-info rdr) => test-vcf-v4_3-meta-info))

(fact "`header` returns header line of the VCF (v4.0) as a vector"
  (with-open [rdr (vcf/reader test-vcf-v4_0-file)]
    (vcf/header rdr) => test-vcf-v4_0-header))

(fact "`header` returns header line of the VCF (v4.3) as a vector"
  (with-open [rdr (vcf/reader test-vcf-v4_3-file)]
    (vcf/header rdr) => test-vcf-v4_3-header))

(fact "`read-variants` returns data lines of the VCF (v4.0) as a lazy sequence"
  (with-open [rdr (vcf/reader test-vcf-v4_0-file)]
    (vcf/read-variants rdr) => test-vcf-v4_0-variants))

(fact "`read-variants` returns data lines of the VCF (v4.3) as a lazy sequence"
  (with-open [rdr (vcf/reader test-vcf-v4_3-file)]
    (vcf/read-variants rdr) => test-vcf-v4_3-variants))

(with-state-changes [(before :facts (prepare-cache!))
                     (after :facts (clean-cache!))]
  (fact "about writing VCF (v4.0)"
    (spit-vcf-for-test temp-file test-vcf-v4_0-meta-info test-vcf-v4_0-header
                       test-vcf-v4_0-variants) => anything
    (slurp-vcf-for-test temp-file) => {:meta-info test-vcf-v4_0-meta-info
                                       :header test-vcf-v4_0-header
                                       :variants test-vcf-v4_0-variants})
  (fact "about writing VCF (v4.3)"
    (spit-vcf-for-test temp-file test-vcf-v4_3-meta-info test-vcf-v4_3-header
                       test-vcf-v4_3-variants) => anything
    (slurp-vcf-for-test temp-file) => {:meta-info test-vcf-v4_3-meta-info
                                       :header test-vcf-v4_3-header
                                       :variants test-vcf-v4_3-variants}))

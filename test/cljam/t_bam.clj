(ns cljam.t-bam
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.bam :as bam]))

(fact "about slurp-bam"
  (bam/slurp test-bam-file) => test-sam)

(with-state-changes [(before :facts (mk-temp-dir!))
                     (after  :facts (rm-temp-dir!))]
  (fact "about spit-bam"
    (let [temp-file (str temp-dir "/test.bam")]
     (bam/spit temp-file test-sam) => nil?
     (bam/slurp temp-file) => test-sam)))

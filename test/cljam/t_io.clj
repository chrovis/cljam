(ns cljam.t-io
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.io :as io]))

(fact "about slurp-sam"
  (io/slurp-sam test-sam-file) => test-sam)

(fact "about slurp-bam"
  (io/slurp-bam test-bam-file) => test-sam)

(with-state-changes [(before :facts (mk-temp-dir!))
                     (after  :facts (rm-temp-dir!))]
  (fact "about spit-sam"
    (let [temp-file (str temp-dir "/test.sam")]
     (io/spit-sam temp-file test-sam) => nil
     (= (slurp temp-file) (slurp test-sam-file)) => truthy))

  (fact "about spit-bam"
    (let [temp-file (str temp-dir "/test.bam")]
     (io/spit-bam temp-file test-sam) => nil
     (= (slurp temp-file) (slurp test-bam-file)) => truthy)))

(fact "about slurp-fasta"
  (io/slurp-fasta test-fa-file) => test-fa)

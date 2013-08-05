(ns cljam.t-fasta-indexer
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.fasta-indexer :as fai]))

(with-state-changes [(before :facts (mk-temp-dir!))
                     (after  :facts (rm-temp-dir!))]
  (fact "about spit"
    (let [temp-file (str temp-dir "/test.fai")]
      (fai/spit temp-file test-fa) => nil?
      (slurp temp-file) => (slurp test-fai-file))))

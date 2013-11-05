(ns cljam.t-sam
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.sam :as sam]
            [cljam.io :as io]))

(fact "about slurp-sam"
  (sam/slurp test-sam-file) => test-sam)

(with-state-changes [(before :facts (mk-temp-dir!))
                     (after  :facts (rm-temp-dir!))]
  (fact "about spit-sam"
    (let [temp-file (str temp-dir "/test.sam")]
     (sam/spit temp-file test-sam) => nil?
     (sam/slurp temp-file) => test-sam)))

(fact "about SAMReader"
      (let [temp-file (str temp-dir "/test.sam")
            rdr (sam/reader temp-file)]
        (io/read-refs rdr) => test-sam-refs))

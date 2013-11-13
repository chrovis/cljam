(ns cljam.t-sam
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.sam :as sam]
            [cljam.io :as io]))

(fact "about slurp-sam"
  (sam/slurp test-sam-file) => test-sam)

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about spit-sam"
    (let [temp-file (str temp-dir "/test.sam")]
     (sam/spit temp-file test-sam) => nil?
     (sam/slurp temp-file) => test-sam)))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (sam/spit (str temp-dir "/test.sam") test-sam)))
                     (after  :facts (clean-cache!))]
  (fact "about SAMReader"
        (let [temp-file (str temp-dir "/test.sam")
              rdr (sam/reader temp-file)]
          (io/read-refs rdr) => test-sam-refs)))

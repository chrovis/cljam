(ns cljam.t-sam
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.io :as io]))

(def temp-file (str temp-dir "/test.sam"))

(fact "about slurp-sam"
      (slurp-sam-for-test test-sam-file) => test-sam)

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about spit-sam"
        (spit-sam-for-test temp-file test-sam) => anything
        (slurp-sam-for-test temp-file) => test-sam))

(with-state-changes [(before :facts (prepare-cache!))
                     (after :facts (clean-cache!))]
  (fact "about spit-sam (medium file)" :slow
        (spit-sam-for-test
          temp-file (slurp-bam-for-test medium-bam-file)) => anything))

;; NB: Cannot spit large SAM (cause `java.lang.OutOfMemoryError`)
;(with-state-changes [(before :facts (do (prepare-cavy!)
;                                        (prepare-cache!)))
;                     (after :facts (clean-cache!))]
;  (fact "about spit-sam (large file)" :slow :heavy
;        (spit-sam-for-test
;          temp-file (slurp-bam-for-test large-bam-file)) => anything))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (spit-sam-for-test temp-file test-sam)))
                     (after  :facts (clean-cache!))]
  (fact "about SAMReader"
        (let [rdr (sam/reader temp-file)]
          (io/read-refs rdr) => test-sam-refs)))

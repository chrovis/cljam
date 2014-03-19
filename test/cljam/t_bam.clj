(ns cljam.t-bam
  (:use midje.sweet
        cljam.t-common)
  (:require [clojure.java.io :refer [copy file]]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.sorter :as sorter]))

(def temp-file (str temp-dir "/test.bam"))
(def temp-file-sorted (str temp-dir "/test.sorted.bam"))

(fact "about slurp-bam"
  (slurp-bam-for-test test-bam-file) => test-sam)

(fact "about slurp-bam (medium file)" :slow
  (slurp-bam-for-test medium-bam-file) => anything)

;; NB: Cannot slurp large BAM (cause `java.lang.OutOfMemoryError`)
;(with-state-changes [(before :facts (prepare-cavia!))]
;  (fact "about slurp-bam (large file)" :slow :heavy
;        (slurp-bam-for-test large-bam-file) => anything))

(with-state-changes [(before :facts (prepare-cache!))
                     (after :facts (clean-cache!))]
  (fact "about spit-bam"
    (spit-bam-for-test temp-file test-sam) => anything
    (slurp-bam-for-test temp-file) => test-sam))

(with-state-changes [(before :facts (prepare-cache!))
                     (after :facts (clean-cache!))]
  (fact "about spit-bam (medium file)" :slow
    (spit-bam-for-test
      temp-file (slurp-bam-for-test medium-bam-file)) => anything))

;; NB: Cannot spit large BAM (cause `java.lang.OutOfMemoryError`)
;(with-state-changes [(before :facts (do (prepare-cavia!)
;                                        (prepare-cache!)))
;                     (after :facts (clean-cache!))]
;  (fact "about spit-bam (large file)" :slow :heavy
;    (spit-bam-for-test
;      temp-file (slurp-bam-for-test large-bam-file)) => anything))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (spit-bam-for-test temp-file test-sam)))
                     (after :facts (clean-cache!))]
  (fact "about BAMReader"
    (let [rdr (bam/reader temp-file :ignore-index true)]
      (io/read-refs rdr) => test-sam-refs))
  (fact "about BAMReader (medium file)" :slow
    (let [rdr (bam/reader medium-bam-file :ignore-index true)]
      (io/read-refs rdr) => medium-sam-refs)))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (prepare-cavia!)))
                     (after :facts (clean-cache!))]
  (fact "about BAMReader (large file)" :slow :heavy
    (let [rdr (bam/reader large-bam-file)]
      (io/read-refs rdr) => large-sam-refs)))

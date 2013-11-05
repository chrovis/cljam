(ns cljam.t-bam
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.bam :as bam]
            [cljam.io :as io]))

(fact "about slurp-bam"
      (with-open [r (bam/reader test-bam-file)]
        {:header (io/read-header r)
         :alignments (doall (io/read-alignments r {}))}) => test-sam)

(with-state-changes [(before :facts (mk-temp-dir!))
                     (after  :facts (rm-temp-dir!))]
  (fact "about spit-bam"
    (let [temp-file (str temp-dir "/test.bam")]
     (bam/spit temp-file test-sam) => nil?
     (with-open [r (bam/reader test-bam-file)]
       {:header (io/read-header r)
        :alignments (doall (io/read-alignments r {}))}) => test-sam)))

(fact "about BAMReader"
      (let [temp-file (str temp-dir "/test.bam")
            rdr (bam/reader temp-file)]
        (io/read-refs rdr) => test-sam-refs))

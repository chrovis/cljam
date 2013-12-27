(ns cljam.t-bam
  (:use midje.sweet
        cljam.t-common)
  (:require [clojure.java.io :refer [copy file]]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.bam-indexer :as bai]))

(fact "about slurp-bam"
      (with-open [r (bam/reader test-bam-file)]
        {:header (io/read-header r)
         :alignments (doall (io/read-alignments r {}))}) => test-sam)

(with-state-changes [(before :facts (prepare-cache!))
                     (after :facts (clean-cache!))]
  (fact "about spit-bam"
    (let [temp-file (str temp-dir "/test.bam")]
     (bam/spit temp-file test-sam) => nil?
     (with-open [r (bam/reader test-bam-file)]
       {:header (io/read-header r)
        :alignments (doall (io/read-alignments r {}))}) => test-sam)))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (bam/spit (str temp-dir "/test.bam") test-sam)))
                     (after :facts (clean-cache!))]
  (fact "about BAMReader"
        (let [temp-file (str temp-dir "/test.bam")
              rdr (bam/reader temp-file)]
          (io/read-refs rdr) => test-sam-refs)))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (copy (file test-sorted-bam-file) (file (str temp-dir "/test.sorted.bam")))))
                     (after :facts (clean-cache!))]
  (fact "about BAM indexer"
        (let [f (str temp-dir "/test.sorted.bam")]
          (bai/create-index f (str f ".bai"))) => anything
        (with-open [r (bam/reader (str temp-dir "/test.sorted.bam"))]
          (io/read-alignments r {:chr "ref" :start 0 :end 1000})) => (filter #(= "ref" (:rname %)) (:alignments test-sam-sorted-by-pos))
        ;; incomplete alignments tests
        (let [f (str temp-dir "/test.incomplete.bam")]
          ;; generate incomplete bam file on the fly
          (bam/spit f test-sam-incomplete-alignments)
          (bai/create-index f (str f ".bai"))) => anything
        ;; TODO: must check .bai files
        ))

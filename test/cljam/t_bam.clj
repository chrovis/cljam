(ns cljam.t-bam
  (:use midje.sweet
        cljam.t-common)
  (:require [clojure.java.io :refer [copy file]]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.bam-indexer :as bai]
            [cljam.sorter :as sorter]))

(fact "about slurp-bam"
      (with-open [r (bam/reader test-bam-file)]
        {:header (io/read-header r)
         :alignments (doall (io/read-alignments r {}))}) => test-sam)

(with-state-changes [(before :facts (prepare-cache!))
                     (after :facts (clean-cache!))]
  (fact "about spit-bam"
        (let [temp-file (str temp-dir "/test.bam")]
          ;; FIXME rewrite
          (bam/spit temp-file test-sam) => nil?
          (with-open [r (bam/reader test-bam-file)]
            {:header (io/read-header r)
             :alignments (doall (io/read-alignments r {}))}) => test-sam)))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (spit-bam-for-test (str temp-dir "/test.bam") test-sam)))
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
        (let [f (str temp-dir "/test.incomplete.bam")
              sorted-f (str temp-dir "/test.incomplete.sorted.bam")]
          ;; generate incomplete bam file on the fly
          (spit-bam-for-test f test-sam-incomplete-alignments)
          (sorter/sort-by-pos (bam/reader f) (bam/writer sorted-f))
          (bai/create-index sorted-f (str sorted-f ".bai"))) => anything
        (with-open [r (bam/reader (str temp-dir "/test.incomplete.sorted.bam"))]
          (io/read-alignments r {:chr "ref" :start 0 :end 1000})) => (filter #(= "ref" (:rname %)) (:alignments test-sam-incomplete-alignments-sorted-by-pos))
        ;; TODO: need more strictly check to .bai files
        ;; (it will use https://gitlab.xcoo.jp/chrovis/cljam/issues/8 later)
        ))

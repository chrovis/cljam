(ns cljam.t-bam
  (:use midje.sweet
        cljam.t-common)
  (:require [clojure.java.io :refer [copy file]]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.bam-indexer :as bai]
            [cljam.sorter :as sorter]))

(def temp-file (str temp-dir "/test.bam"))
(def temp-file-sorted (str temp-dir "/test.sorted.bam"))

(fact "about slurp-bam"
      (slurp-bam-for-test test-bam-file) => test-sam)

(with-state-changes [(before :facts (prepare-cache!))
                     (after :facts (clean-cache!))]
  (fact "about spit-bam"
        (spit-bam-for-test temp-file test-sam) => anything
        (slurp-bam-for-test temp-file) => test-sam))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (spit-bam-for-test temp-file test-sam)))
                     (after :facts (clean-cache!))]
  (fact "about BAMReader"
        (let [rdr (bam/reader temp-file)]
          (io/read-refs rdr) => test-sam-refs)))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (copy (file test-sorted-bam-file) (file temp-file-sorted))))
                     (after :facts (clean-cache!))]
  (fact "about BAM indexer"
        (bai/create-index temp-file-sorted (str temp-file-sorted ".bai")) => anything
        (with-open [r (bam/reader temp-file-sorted)]
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

(ns cljam.t-bam-indexer
  "Tests for cljam.bam-indexer."
  (:require [midje.sweet :refer :all]
            [me.raynes.fs :as fs]
            [cljam.t-common :refer :all]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.sorter :as sorter]
            [cljam.bam-indexer :as bai]))

(def temp-file-sorted (str temp-dir "/test.sorted.bam"))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (fs/copy test-sorted-bam-file
                                                 temp-file-sorted)))
                     (after :facts (clean-cache!))]
  (fact "about BAM indexer"
    (bai/create-index
      temp-file-sorted (str temp-file-sorted ".bai")) => anything
    (fs/exists? (str temp-file-sorted ".bai")) => truthy
    (same-file? (str temp-file-sorted ".bai") test-bai-file) => truthy
    (with-open [r (bam/reader temp-file-sorted)]
      (io/read-alignments r {:chr "ref" :start 0 :end 1000})
      ) => (filter #(= "ref" (:rname %))
                   (:alignments test-sam-sorted-by-pos))))

(let [f (str temp-dir "/test.incomplete.bam")
      sorted-f (str temp-dir "/test.incomplete.sorted.bam")]
  (with-state-changes [(before :facts (do (prepare-cache!)
                                          ;; generate incomplete bam on the fly
                                          (spit-bam-for-test
                                            f test-sam-incomplete-alignments)
                                          ;; TODO: go independent from sorter
                                          (sorter/sort-by-pos
                                            (bam/reader f :ignore-index true)
                                            (bam/writer sorted-f))))
                       (after :facts (clean-cache!))]
    (fact "about BAM indexer (for incomplete alignments)"
      (bai/create-index sorted-f (str sorted-f ".bai")) => anything
      (fs/exists? (str sorted-f ".bai")) => truthy
      (with-open [r (bam/reader sorted-f)]
        (io/read-alignments r {:chr "ref" :start 0 :end 1000})
        ) => (filter #(= "ref" (:rname %))
                     (:alignments test-sam-incomplete-alignments-sorted-by-pos))
      ;; TODO: need more strictly check to .bai files
      ;; (it will use https://gitlab.xcoo.jp/chrovis/cljam/issues/8 later)
      )))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (fs/copy medium-bam-file
                                                 temp-file-sorted)))
                     (after :facts (clean-cache!))]
  (fact "about BAM indexer (medium file)" :slow
    (bai/create-index
      temp-file-sorted (str temp-file-sorted ".bai")) => anything
    (fs/exists? (str temp-file-sorted ".bai")) => truthy))

(with-state-changes [(before :facts (do (prepare-cavia!)
                                        (prepare-cache!)
                                        (fs/copy large-bam-file
                                                 temp-file-sorted)))
                     (after :facts (clean-cache!))]
  (fact "about BAM indexer (large file)" :slow :heavy
    (bai/create-index
      temp-file-sorted (str temp-file-sorted ".bai")) => anything
    (fs/exists? (str temp-file-sorted ".bai")) => truthy))

(ns cljam.t-bam-indexer
  (:require [clojure.java.io :refer [copy file]]
            [midje.sweet :refer :all]
            [cljam.t-common :refer :all]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.sorter :as sorter]
            [cljam.bam-indexer :as bai]))

(def temp-file-sorted (str temp-dir "/test.sorted.bam"))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (copy (file test-sorted-bam-file)
                                              (file temp-file-sorted))))
                     (after :facts (clean-cache!))]
  (fact "about BAM indexer"
    (bai/create-index!
      temp-file-sorted (str temp-file-sorted ".bai")) => anything
    (.exists (file (str temp-file-sorted ".bai"))) => truthy
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
      (bai/create-index! sorted-f (str sorted-f ".bai")) => anything
      (.exists (file (str sorted-f ".bai"))) => truthy
      (with-open [r (bam/reader sorted-f)]
        (io/read-alignments r {:chr "ref" :start 0 :end 1000})
        ) => (filter #(= "ref" (:rname %))
                     (:alignments test-sam-incomplete-alignments-sorted-by-pos))
      ;; TODO: need more strictly check to .bai files
      ;; (it will use https://gitlab.xcoo.jp/chrovis/cljam/issues/8 later)
      )))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (copy (file medium-bam-file)
                                              (file temp-file-sorted))))
                     (after :facts (clean-cache!))]
  (fact "about BAM indexer (medium file)" :slow
    (bai/create-index!
      temp-file-sorted (str temp-file-sorted ".bai")) => anything
    (.exists (file (str temp-file-sorted ".bai"))) => truthy))

(with-state-changes [(before :facts (do (prepare-cavia!)
                                        (prepare-cache!)
                                        (copy (file large-bam-file)
                                              (file temp-file-sorted))))
                     (after :facts (clean-cache!))]
  (fact "about BAM indexer (large file)" :slow :heavy
    (bai/create-index!
      temp-file-sorted (str temp-file-sorted ".bai")) => anything
    (.exists (file (str temp-file-sorted ".bai"))) => truthy))

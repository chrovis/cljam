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

(fact "about slurp-bam (medium file)" :slow
  (slurp-bam-for-test medium-bam-file) => anything)

;; NB: Cannot slurp large BAM (cause `java.lang.OutOfMemoryError`)
;(with-state-changes [(before :facts (prepare-cavy!))]
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
;(with-state-changes [(before :facts (do (prepare-cavy!)
;                                        (prepare-cache!)))
;                     (after :facts (clean-cache!))]
;  (fact "about spit-bam (large file)" :slow :heavy
;    (spit-bam-for-test
;      temp-file (slurp-bam-for-test large-bam-file)) => anything))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (spit-bam-for-test temp-file test-sam)))
                     (after :facts (clean-cache!))]
  (fact "about BAMReader"
    (let [rdr (bam/reader temp-file)]
      (io/read-refs rdr) => test-sam-refs))
  (fact "about BAMReader (medium file)" :slow
    (let [rdr (bam/reader medium-bam-file)]
      (io/read-refs rdr) => medium-sam-refs)))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (prepare-cavy!)))
                     (after :facts (clean-cache!))]
  (fact "about BAMReader (large file)" :slow :heavy
    (let [rdr (bam/reader large-bam-file)]
      (io/read-refs rdr) => large-sam-refs)))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (copy (file test-sorted-bam-file)
                                              (file temp-file-sorted))))
                     (after :facts (clean-cache!))]
  (fact "about BAM indexer"
    (bai/create-index
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
                                            (bam/reader f)
                                            (bam/writer sorted-f))))
                       (after :facts (clean-cache!))]
    (fact "about BAM indexer (for incomplete alignments)"
      (bai/create-index sorted-f (str sorted-f ".bai")) => anything
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
    (bai/create-index
      temp-file-sorted (str temp-file-sorted ".bai")) => anything
    (.exists (file (str temp-file-sorted ".bai"))) => truthy))

(with-state-changes [(before :facts (do (prepare-cavy!)
                                        (prepare-cache!)
                                        (copy (file large-bam-file)
                                              (file temp-file-sorted))))
                     (after :facts (clean-cache!))]
  (fact "about BAM indexer (large file)" :slow :heavy
    (bai/create-index
      temp-file-sorted (str temp-file-sorted ".bai")) => anything
    (.exists (file (str temp-file-sorted ".bai"))) => truthy))


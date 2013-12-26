(ns cljam.t-sorter
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.sorter :as sorter]))

(def tmp-coordinate-sorted-sam-file (str temp-dir "/" "tmp.coordinate.sorted.sam"))
(def tmp-coordinate-sorted-bam-file (str temp-dir "/" "tmp.coordinate.sorted.bam"))

(def tmp-queryname-sorted-sam-file (str temp-dir "/" "tmp.queryname.sorted.sam"))
(def tmp-queryname-sorted-bam-file (str temp-dir "/" "tmp.queryname.sorted.bam"))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about sorting a sam by chromosomal positions"
      (sorter/sort-by-pos (sam/reader test-sam-file)
                          (sam/writer tmp-coordinate-sorted-sam-file)) => nil
      (sorter/sort-by-pos (bam/reader test-bam-file)
                          (bam/writer tmp-coordinate-sorted-bam-file)) => nil
      (sorter/sort-by-qname (sam/reader test-sam-file)
                            (sam/writer tmp-queryname-sorted-sam-file)) =future=> nil
      (sorter/sort-by-qname (bam/reader test-bam-file)
                            (bam/writer tmp-queryname-sorted-bam-file)) =future=> nil
      (sorter/sorted-by? (sam/reader test-sam-file)) => falsey
      (sorter/sorted-by? (sam/reader tmp-coordinate-sorted-sam-file)) => truthy
      (sorter/sorted-by? (bam/reader tmp-coordinate-sorted-bam-file)) => truthy
      (sorter/sorted-by? (sam/reader tmp-queryname-sorted-sam-file)) =future=> truthy
      (sorter/sorted-by? (bam/reader tmp-queryname-sorted-bam-file)) =future=> truthy
      (sorter/sort-order (sam/reader test-sam-file)) => sorter/order-unknown
      (sorter/sort-order (sam/reader tmp-coordinate-sorted-sam-file)) => sorter/order-coordinate
      (sorter/sort-order (bam/reader tmp-coordinate-sorted-bam-file)) => sorter/order-coordinate
      (sorter/sort-order (sam/reader tmp-queryname-sorted-sam-file)) =future=> sorter/order-queryname
      (sorter/sort-order (bam/reader tmp-queryname-sorted-bam-file)) =future=> sorter/order-queryname))

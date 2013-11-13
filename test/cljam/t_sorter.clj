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
      (sorter/sorted? (sam/reader test-sam-file)) => falsey
      (sorter/sorted? (sam/reader tmp-coordinate-sorted-sam-file)) => truthy
      (sorter/sorted? (bam/reader tmp-coordinate-sorted-bam-file)) => truthy
      (sorter/sort-order (sam/reader test-sam-file)) => sorter/order-unknown
      (sorter/sort-order (sam/reader tmp-coordinate-sorted-sam-file)) => sorter/order-coordinate
      (sorter/sort-order (bam/reader tmp-coordinate-sorted-bam-file)) => sorter/order-coordinate))

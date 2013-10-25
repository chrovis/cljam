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

(fact "about sorting a sam by chromosomal positions"
      (sorter/sort-by-pos (sam/reader test-sam-file)
                          (sam/writer tmp-coordinate-sorted-sam-file)) => nil
      (sorter/sort-by-pos (bam/reader test-bam-file)
                          (bam/writer tmp-coordinate-sorted-bam-file)) => nil)

(fact "about sorting a sam by read names"
      (sorter/sort-by-qname (sam/reader test-sam-file)
                            (sam/writer tmp-queryname-sorted-sam-file)) => nil
      (sorter/sort-by-qname (bam/reader test-bam-file)
                            (bam/writer tmp-queryname-sorted-bam-file)) => nil)

(fact "about sorted?"
      (sorter/sorted? (sam/reader test-sam-file)) => falsey
      (sorter/sorted? (sam/reader tmp-coordinate-sorted-sam-file)) => truthy
      (sorter/sorted? (bam/reader tmp-coordinate-sorted-bam-file)) => truthy)

(fact "about sort-order"
      (sorter/sort-order (sam/reader test-sam-file)) => sorter/order-unknown
      (sorter/sort-order (bam/reader tmp-coordinate-sorted-bam-file)) => sorter/order-coordinate
      (sorter/sort-order (bam/reader tmp-queryname-sorted-bam-file)) => sorter/order-queryname)

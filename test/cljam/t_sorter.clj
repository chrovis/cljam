(ns cljam.t-sorter
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.sorter :as sorter]))

(def tmp-shuffled-sam-file (str temp-dir "/" "tmp.shuffled.sam"))
(def tmp-shuffled-bam-file (str temp-dir "/" "tmp.shuffled.bam"))

(def tmp-coordinate-sorted-sam-file (str temp-dir "/" "tmp.coordinate.sorted.sam"))
(def tmp-coordinate-sorted-bam-file (str temp-dir "/" "tmp.coordinate.sorted.bam"))

(def tmp-queryname-sorted-sam-file (str temp-dir "/" "tmp.queryname.sorted.sam"))
(def tmp-queryname-sorted-bam-file (str temp-dir "/" "tmp.queryname.sorted.bam"))

(defn prepare-shuffled-files!
  []
  (sam/spit tmp-shuffled-sam-file (get-shuffled-test-sam))
  (bam/spit tmp-shuffled-bam-file (get-shuffled-test-sam)))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (prepare-shuffled-files!)))
                     (after  :facts (clean-cache!))]
  (fact "about sorting a sam by chromosomal positions"
        ;; tests by test-sam-file and test-bam-file
        (sorter/sort-by-pos (sam/reader test-sam-file)
                            (sam/writer tmp-coordinate-sorted-sam-file)) => anything
        (sorter/sort-by-pos (bam/reader test-bam-file)
                            (bam/writer tmp-coordinate-sorted-bam-file)) => anything
        (sorter/sort-by-qname (sam/reader test-sam-file)
                              (sam/writer tmp-queryname-sorted-sam-file)) =future=> anything
        (sorter/sort-by-qname (bam/reader test-bam-file)
                              (bam/writer tmp-queryname-sorted-bam-file)) =future=> anything
        (sorter/sorted-by? (sam/reader test-sam-file)) => falsey
        (sorter/sorted-by? (bam/reader test-bam-file)) => falsey
        (sorter/sorted-by? (sam/reader tmp-coordinate-sorted-sam-file)) => truthy
        (sorter/sorted-by? (bam/reader tmp-coordinate-sorted-bam-file)) => truthy
        (sorter/sorted-by? (sam/reader tmp-queryname-sorted-sam-file)) =future=> truthy
        (sorter/sorted-by? (bam/reader tmp-queryname-sorted-bam-file)) =future=> truthy
        (sorter/sort-order (sam/reader test-sam-file)) => sorter/order-unknown
        (sorter/sort-order (bam/reader test-bam-file)) => sorter/order-unknown
        (sorter/sort-order (sam/reader tmp-coordinate-sorted-sam-file)) => sorter/order-coordinate
        (sorter/sort-order (bam/reader tmp-coordinate-sorted-bam-file)) => sorter/order-coordinate
        (sorter/sort-order (sam/reader tmp-queryname-sorted-sam-file)) =future=> sorter/order-queryname
        (sorter/sort-order (bam/reader tmp-queryname-sorted-bam-file)) =future=> sorter/order-queryname
        ;; tests by shuffled files (its may sorted by chance)
        (sorter/sort-by-pos (sam/reader tmp-shuffled-sam-file)
                            (sam/writer tmp-coordinate-sorted-sam-file)) => anything
        (sorter/sort-by-pos (bam/reader tmp-shuffled-bam-file)
                            (bam/writer tmp-coordinate-sorted-bam-file)) => anything
        (sorter/sort-by-qname (sam/reader tmp-shuffled-sam-file)
                              (sam/writer tmp-queryname-sorted-sam-file)) =future=> anything
        (sorter/sort-by-qname (bam/reader tmp-shuffled-bam-file)
                              (bam/writer tmp-queryname-sorted-bam-file)) =future=> anything
        (sorter/sorted-by? (sam/reader tmp-shuffled-sam-file)) => anything
        (sorter/sorted-by? (bam/reader tmp-shuffled-bam-file)) => anything
        (sorter/sorted-by? (sam/reader tmp-coordinate-sorted-sam-file)) => truthy
        (sorter/sorted-by? (bam/reader tmp-coordinate-sorted-bam-file)) => truthy
        (sorter/sorted-by? (sam/reader tmp-queryname-sorted-sam-file)) =future=> truthy
        (sorter/sorted-by? (bam/reader tmp-queryname-sorted-bam-file)) =future=> truthy
        (sorter/sort-order (sam/reader tmp-shuffled-sam-file)) => #(#{:queryname :coordinate :unsorted :unknown} %)
        (sorter/sort-order (bam/reader tmp-shuffled-bam-file)) => #(#{:queryname :coordinate :unsorted :unknown} %)
        (sorter/sort-order (sam/reader tmp-coordinate-sorted-sam-file)) => sorter/order-coordinate
        (sorter/sort-order (bam/reader tmp-coordinate-sorted-bam-file)) => sorter/order-coordinate
        (sorter/sort-order (sam/reader tmp-queryname-sorted-sam-file)) =future=> sorter/order-queryname
        (sorter/sort-order (bam/reader tmp-queryname-sorted-bam-file)) =future=> sorter/order-queryname
        ))

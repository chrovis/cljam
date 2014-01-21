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

(def tmp-shuffled-sam-file (str temp-dir "/" "tmp.shuffled.sam"))
(def tmp-shuffled-bam-file (str temp-dir "/" "tmp.shuffled.bam"))

(def tmp-coordinate-sorted-sam-file-2 (str temp-dir "/" "tmp.coordinate.sorted.2.sam"))
(def tmp-coordinate-sorted-bam-file-2 (str temp-dir "/" "tmp.coordinate.sorted.2.bam"))

(def tmp-queryname-sorted-sam-file-2 (str temp-dir "/" "tmp.queryname.sorted.2.sam"))
(def tmp-queryname-sorted-bam-file-2 (str temp-dir "/" "tmp.queryname.sorted.2.bam"))

(defn- prepare-shuffled-files!
  []
  (spit-sam-for-test tmp-shuffled-sam-file (get-shuffled-test-sam))
  (spit-bam-for-test tmp-shuffled-bam-file (get-shuffled-test-sam)))

(defn- with-reader
  [target-fn src-file & [dst-file]]
  (let [src-reader (cond
                     (re-find #"\.sam$" src-file) sam/reader
                     (re-find #"\.bam$" src-file) bam/reader
                     :else (throw (RuntimeException.
                                    (str "invalid file suffix " src-file))))
        dst-writer (cond
                     (not dst-file) nil
                     (re-find #"\.sam$" dst-file) sam/writer
                     (re-find #"\.bam$" dst-file) bam/writer
                     :else (throw (RuntimeException.
                                    (str "invalid file suffix " dst-file))))]
    (if dst-file
      (target-fn (src-reader src-file) (dst-writer dst-file))
      (target-fn (src-reader src-file)))))


(with-state-changes [(before :facts (do (prepare-cache!)
                                        (prepare-shuffled-files!)))
                     (after  :facts (clean-cache!))]
  (fact "about sorting a sam by chromosomal positions"
        ;; tests by test-sam-file and test-bam-file
        (with-reader sorter/sort-by-pos test-sam-file tmp-coordinate-sorted-sam-file) => anything
        (with-reader sorter/sort-by-pos test-bam-file tmp-coordinate-sorted-bam-file) => anything
        (with-reader sorter/sort-by-qname test-sam-file tmp-queryname-sorted-sam-file) =future=> anything
        (with-reader sorter/sort-by-qname test-bam-file tmp-queryname-sorted-bam-file) =future=> anything
        (with-reader sorter/sorted-by? test-sam-file) => falsey
        (with-reader sorter/sorted-by? test-bam-file) => falsey
        (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-file) => truthy
        (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file) => truthy
        (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file) =future=> truthy
        (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file) =future=> truthy
        (with-reader sorter/sort-order test-sam-file) => sorter/order-unknown
        (with-reader sorter/sort-order test-bam-file) => sorter/order-unknown
        (with-reader sorter/sort-order tmp-coordinate-sorted-sam-file) => sorter/order-coordinate
        (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file) => sorter/order-coordinate
        (with-reader sorter/sort-order tmp-queryname-sorted-sam-file) =future=> sorter/order-queryname
        (with-reader sorter/sort-order tmp-queryname-sorted-bam-file) =future=> sorter/order-queryname
        ;; tests by shuffled files (its may sorted by chance)
        (with-reader sorter/sort-by-pos tmp-shuffled-sam-file tmp-coordinate-sorted-sam-file-2) => anything
        (with-reader sorter/sort-by-pos tmp-shuffled-bam-file tmp-coordinate-sorted-bam-file-2) => anything
        (with-reader sorter/sort-by-qname tmp-shuffled-sam-file tmp-coordinate-sorted-sam-file-2) =future=> anything
        (with-reader sorter/sort-by-qname tmp-shuffled-bam-file tmp-coordinate-sorted-bam-file-2) =future=> anything
        (with-reader sorter/sorted-by? tmp-shuffled-sam-file) => anything
        (with-reader sorter/sorted-by? tmp-shuffled-bam-file) => anything
        (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-file-2) => truthy
        (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file-2) => truthy
        (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file-2) =future=> truthy
        (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file-2) =future=> truthy
        (with-reader sorter/sort-order tmp-shuffled-sam-file) => #(#{:queryname :coordinate :unsorted :unknown} %)
        (with-reader sorter/sort-order tmp-shuffled-bam-file) => #(#{:queryname :coordinate :unsorted :unknown} %)
        (with-reader sorter/sort-order tmp-coordinate-sorted-sam-file-2) => sorter/order-coordinate
        (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file-2) => sorter/order-coordinate
        (with-reader sorter/sort-order tmp-queryname-sorted-sam-file-2) =future=> sorter/order-queryname
        (with-reader sorter/sort-order tmp-queryname-sorted-bam-file-2) =future=> sorter/order-queryname
        ;; check sorting order
        (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-sam-file-2) (slurp-sam-for-test tmp-coordinate-sorted-sam-file)) => anything
        (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-file-2) (slurp-bam-for-test tmp-coordinate-sorted-bam-file)) => anything
        ;; compare generated files
        (slurp-sam-for-test tmp-coordinate-sorted-sam-file-2) => (slurp-sam-for-test tmp-coordinate-sorted-sam-file)
        (slurp-bam-for-test tmp-coordinate-sorted-bam-file-2) => (slurp-bam-for-test tmp-coordinate-sorted-bam-file)
        (slurp-sam-for-test tmp-queryname-sorted-sam-file-2) =future=> (slurp-sam-for-test tmp-queryname-sorted-sam-file)
        (slurp-bam-for-test tmp-queryname-sorted-bam-file-2) =future=> (slurp-bam-for-test tmp-queryname-sorted-bam-file)
        ))

(with-state-changes [(before :facts (prepare-cache!))
                     (after  :facts (clean-cache!))]
  (fact "about sorting (medium file)" :slow
        (with-reader sorter/sort-by-pos medium-bam-file tmp-coordinate-sorted-sam-file) => anything
        (with-reader sorter/sort-by-pos medium-bam-file tmp-coordinate-sorted-bam-file) => anything
        (with-reader sorter/sort-by-qname medium-bam-file tmp-queryname-sorted-sam-file) =future=> anything
        (with-reader sorter/sort-by-qname medium-bam-file tmp-queryname-sorted-bam-file) =future=> anything
        (with-reader sorter/sorted-by? medium-bam-file) => falsey
        (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-file) => truthy
        (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file) => truthy
        (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file) =future=> truthy
        (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file) =future=> truthy
        (with-reader sorter/sort-order medium-bam-file) => sorter/order-unknown
        (with-reader sorter/sort-order tmp-coordinate-sorted-sam-file) => sorter/order-coordinate
        (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file) => sorter/order-coordinate
        (with-reader sorter/sort-order tmp-queryname-sorted-sam-file) =future=> sorter/order-queryname
        (with-reader sorter/sort-order tmp-queryname-sorted-bam-file) =future=> sorter/order-queryname
        ;; check sorting order
        (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-sam-file)) => anything
        (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-file)) => anything
        ))

(with-state-changes [(before :facts (do (prepare-cavy!)
                                        (prepare-cache!)))
                     (after  :facts (clean-cache!))]
  (fact "about sorting (large file)" :slow :heavy
        (with-reader sorter/sort-by-pos large-bam-file tmp-coordinate-sorted-sam-file) => anything
        (with-reader sorter/sort-by-pos large-bam-file tmp-coordinate-sorted-bam-file) => anything
        (with-reader sorter/sort-by-qname large-bam-file tmp-queryname-sorted-sam-file) =future=> anything
        (with-reader sorter/sort-by-qname large-bam-file tmp-queryname-sorted-bam-file) =future=> anything
        ;(with-reader sorter/sorted-by? large-bam-file) => falsey
        (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-file) => truthy
        (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file) => truthy
        (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file) =future=> truthy
        (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file) =future=> truthy
        ;(with-reader sorter/sort-order large-bam-file) => sorter/order-unknown
        (with-reader sorter/sort-order tmp-coordinate-sorted-sam-file) => sorter/order-coordinate
        (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file) => sorter/order-coordinate
        (with-reader sorter/sort-order tmp-queryname-sorted-sam-file) =future=> sorter/order-queryname
        (with-reader sorter/sort-order tmp-queryname-sorted-bam-file) =future=> sorter/order-queryname
        ;; check sorting order
        ;(check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-sam-file)) => anything
        ;(check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-file)) => anything
        ))

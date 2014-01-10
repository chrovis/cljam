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

(defn- uniq [coll]
  (reduce
    (fn [r one]
      (if (= (first r) one)
        r
        (conj r one)))
    nil
    coll))
(defn- get-rnames [sam]
  (uniq (map :rname (:alignments sam))))

(defn- check-sort-order [target-sam & [contrast-sam]]
  ;; TODO: only coordinate currently. need to test by queryname sort.
  (let [target-rnames (get-rnames target-sam)]
    ;; check rname groups
    (when contrast-sam
      (when-not (= target-rnames (get-rnames contrast-sam))
        (throw (Exception. "not matched by rnames order"))))
    ;; check order
    (dorun
      (map
        (fn [rname]
          (reduce
            (fn [prev one]
              (case (compare (:pos prev) (:pos one))
                -1 true
                1 (throw (Exception. "pos not sorted"))
                (case (compare (:qname prev) (:qname one))
                  -1 true
                  1 (throw (Exception. "qname not sorted"))
                  true))
              one)
            (filter #(= rname (:rname %)) (:alignments target-sam))))
        target-rnames))))

(defn- prepare-shuffled-files!
  []
  (spit-sam-for-test tmp-shuffled-sam-file (get-shuffled-test-sam))
  (spit-bam-for-test tmp-shuffled-bam-file (get-shuffled-test-sam)))

(defn- with-reader
  [target-fn src-file & [dst-file]]
  (cond
    (re-find #"\.sam$" src-file) (if dst-file
                                   (target-fn (sam/reader src-file)
                                              (sam/writer dst-file))
                                   (target-fn (sam/reader src-file)))
    (re-find #"\.bam$" src-file) (if dst-file
                                   (target-fn (bam/reader src-file)
                                              (bam/writer dst-file))
                                   (target-fn (bam/reader src-file)))
    :else (throw (RuntimeException. (str "invalid file suffix " src-file)))))


(with-state-changes [(before :facts (do (prepare-cache!)
                                        (prepare-shuffled-files!)))
                     (after  :facts (clean-cache!))]
  (fact "about sorting a sam by chromosomal positions"
        ;; tests by test-sam-file and test-bam-file
        (with-reader sorter/sort-by-pos test-sam-file tmp-coordinate-sorted-sam-file) => anything
        (with-reader sorter/sort-by-pos test-bam-file tmp-coordinate-sorted-bam-file) => anything
        (with-reader sorter/sort-by-qname test-sam-file tmp-coordinate-sorted-sam-file) =future=> anything
        (with-reader sorter/sort-by-qname test-bam-file tmp-coordinate-sorted-bam-file) =future=> anything
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

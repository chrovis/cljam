(ns cljam.t-sorter
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.sorter :as sorter])
  (:import [java.io Closeable]))

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
  (let [src-rdr (cond
                 (re-find #"\.sam$" src-file) (sam/reader src-file)
                 (re-find #"\.bam$" src-file) (bam/reader src-file :ignore-index true)
                 :else (throw (RuntimeException.
                               (str "invalid file suffix " src-file))))
        dst-wtr (if dst-file
                  (cond
                   (not dst-file) nil
                   (re-find #"\.sam$" dst-file) (sam/writer dst-file)
                   (re-find #"\.bam$" dst-file) (bam/writer dst-file)
                   :else (throw (RuntimeException.
                                 (str "invalid file suffix " dst-file))))
                  nil)]
    (if-not (nil? dst-wtr)
      (with-open [src ^Closeable src-rdr
                  dst ^Closeable dst-wtr] (target-fn src dst))
      (with-open [src ^Closeable src-rdr] (target-fn src)))))

(deftest about-sorting-a-sam-by-chromosomal-positions
  (with-before-after {:before (do (prepare-cache!)
                                  (prepare-shuffled-files!))
                      :after (clean-cache!)}
    ;; tests by test-sam-file and test-bam-file
    (is (thrown? Exception
                 (with-reader sorter/sort-by-pos test-sam-file tmp-coordinate-sorted-sam-file)))
    (is (not-throw? (with-reader sorter/sort-by-pos test-bam-file tmp-coordinate-sorted-bam-file)))
    ;; (is (not-throw? (with-reader sorter/sort-by-qname test-sam-file tmp-queryname-sorted-sam-file))) ; TODO: future
    ;; (is (not-throw? (with-reader sorter/sort-by-qname test-bam-file tmp-queryname-sorted-bam-file))) ; TODO: future
    (is (not (with-reader sorter/sorted-by? test-sam-file)))
    (is (not (with-reader sorter/sorted-by? test-bam-file)))
    (is (not (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-file)))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file))
    ;; (is (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file)) ; TODO: future
    ;; (is (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file)) ; TODO: future
    (is (= (with-reader sorter/sort-order test-sam-file) sorter/order-unknown))
    (is (= (with-reader sorter/sort-order test-bam-file) sorter/order-unknown))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-sam-file)
           sorter/order-unknown))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file)
           sorter/order-coordinate))
    ;; (is (= (with-reader sorter/sort-order tmp-queryname-sorted-sam-file)
    ;;        sorter/order-queryname)) ; TODO: future
    ;; (is (= (with-reader sorter/sort-order tmp-queryname-sorted-bam-file)
    ;;        sorter/order-queryname)) ; TODO: future
    ;; tests by shuffled files (its may sorted by chance)
    (is (thrown? Exception
                 (with-reader sorter/sort-by-pos tmp-shuffled-sam-file tmp-coordinate-sorted-sam-file-2)))
    (is (nil? (with-reader sorter/sort-by-pos tmp-shuffled-bam-file tmp-coordinate-sorted-bam-file-2)))
    ;; (is (not-throw? (with-reader sorter/sort-by-qname tmp-shuffled-sam-file tmp-coordinate-sorted-sam-file-2))) ; TODO: future
    ;; (is (not-throw? (with-reader sorter/sort-by-qname tmp-shuffled-bam-file tmp-coordinate-sorted-bam-file-2))) ; TODO: future
    (is (not-throw? (with-reader sorter/sorted-by? tmp-shuffled-sam-file)))
    (is (not-throw? (with-reader sorter/sorted-by? tmp-shuffled-bam-file)))
    (is (not (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-file-2)))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file-2))
    ;; (is (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file-2)) ; TODO: future
    ;; (is (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file-2)) ; TODO: future
    (is (get #{:queryname :coordinate :unsorted :unknown}
             (with-reader sorter/sort-order tmp-shuffled-sam-file)))
    (is (get #{:queryname :coordinate :unsorted :unknown}
             (with-reader sorter/sort-order tmp-shuffled-bam-file)))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-sam-file-2)
           sorter/order-unknown))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file-2)
           sorter/order-coordinate))
    ;; (is (= (with-reader sorter/sort-order tmp-queryname-sorted-sam-file-2)
    ;;        sorter/order-queryname)) ; TODO: future
    ;; (is (= (with-reader sorter/sort-order tmp-queryname-sorted-bam-file-2)
    ;;        sorter/order-queryname)) ; TODO: future
    ;; check sorting order
    (is (not-throw? (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-sam-file-2) (slurp-sam-for-test tmp-coordinate-sorted-sam-file))))
    (is (not-throw? (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-file-2) (slurp-bam-for-test tmp-coordinate-sorted-bam-file))))
    ;; compare generated files
    (is (= (slurp-sam-for-test tmp-coordinate-sorted-sam-file-2)
           (slurp-sam-for-test tmp-coordinate-sorted-sam-file)))
    (is (= (slurp-bam-for-test tmp-coordinate-sorted-bam-file-2)
           (slurp-bam-for-test tmp-coordinate-sorted-bam-file)))
    ;; (is (= (slurp-sam-for-test tmp-queryname-sorted-sam-file-2)
    ;;        (slurp-sam-for-test tmp-queryname-sorted-sam-file))) ; TODO: future
    ;; (is (= (slurp-bam-for-test tmp-queryname-sorted-bam-file-2)
    ;;        (slurp-bam-for-test tmp-queryname-sorted-bam-file))) ; TODO: future
    ;; TODO: Add test sorter/sort-by-pos with bam file includes many blocks
    ;;       (Currently, testing bam files have only one block)
    ;; (is (not-throw? (with-reader sorter/sort-by-pos many-blocks-bam-file (str temp-dir "/many.bam"))))
    ))

(deftest-slow about-sorting-medium-file
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (thrown? Exception
                 (with-reader sorter/sort-by-pos medium-bam-file tmp-coordinate-sorted-sam-file)))
    (is (nil? (with-reader sorter/sort-by-pos medium-bam-file tmp-coordinate-sorted-bam-file)))
    ;; (is (not-throw? (with-reader sorter/sort-by-qname medium-bam-file tmp-queryname-sorted-sam-file))) ; TODO: future
    ;; (is (not-throw? (with-reader sorter/sort-by-qname medium-bam-file tmp-queryname-sorted-bam-file))) ; TODO: future
    (is (not (with-reader sorter/sorted-by? medium-bam-file)))
    (is (not (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-file)))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file))
    ;; (is (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file)) ; TODO: future
    ;; (is (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file)) ; TODO: future
    (is (= (with-reader sorter/sort-order medium-bam-file)
           sorter/order-unknown))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-sam-file)
           sorter/order-unknown))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file)
           sorter/order-coordinate))
    ;; (is (= (with-reader sorter/sort-order tmp-queryname-sorted-sam-file)
    ;;        sorter/order-queryname)) ; TODO: future
    ;; (is (= (with-reader sorter/sort-order tmp-queryname-sorted-bam-file)
    ;;        sorter/order-queryname)) ; TODO: future
    ;; check sorting order
    (is (not-throw? (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-sam-file))))
    (is (not-throw? (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-file))))))

(deftest-remote about-sorting-large-file
  (with-before-after {:before (do (prepare-cavia!)
                                  (prepare-cache!))
                      :after (clean-cache!)}
    (is (nil? (with-reader sorter/sort-by-pos large-bam-file tmp-coordinate-sorted-bam-file)))
    ;; (is (not-throw? (with-reader sorter/sort-by-qname large-bam-file tmp-queryname-sorted-bam-file))) ; TODO: future
    ;; (is (not (with-reader sorter/sorted-by? large-bam-file)))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file))
    ;; (is (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file)) ; TODO: future
    ;; (is (= (with-reader sorter/sort-order large-bam-file) sorter/order-unknown))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file)
           sorter/order-coordinate))
    ;; (is (= (with-reader sorter/sort-order tmp-queryname-sorted-bam-file)
    ;;        sorter/order-queryname)) ; TODO: future
    ;; check sorting order
    ;; (is (not-throw? (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-file))))
    ))

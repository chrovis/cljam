(ns cljam.algo.sorter-test
  (:require [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.sam :as sam]
            [cljam.algo.sorter :as sorter]
            [cljam.io.sam.util.header :as header])
  (:import [java.io Closeable]))

(use-fixtures :once disable-log-fixture)

(def tmp-coordinate-sorted-sam-file (str temp-dir "/" "tmp.coordinate.sorted.sam"))
(def tmp-coordinate-sorted-bam-file (str temp-dir "/" "tmp.coordinate.sorted.bam"))

(def tmp-queryname-sorted-sam-file (str temp-dir "/" "tmp.queryname.sorted.sam"))
(def tmp-queryname-sorted-bam-file (str temp-dir "/" "tmp.queryname.sorted.bam"))

(def tmp-shuffled-sam-file (str temp-dir "/" "tmp.shuffled.sam"))
(def tmp-shuffled-bam-file (str temp-dir "/" "tmp.shuffled.bam"))

(def tmp-coordinate-sorted-sam-sam-file (str temp-dir "/" "tmp.coordinate.sorted.2.sam.sam"))
(def tmp-coordinate-sorted-sam-bam-file (str temp-dir "/" "tmp.coordinate.sorted.2.sam.bam"))
(def tmp-coordinate-sorted-bam-bam-file (str temp-dir "/" "tmp.coordinate.sorted.2.bam.bam"))
(def tmp-coordinate-sorted-bam-sam-file (str temp-dir "/" "tmp.coordinate.sorted.2.bam.sam"))

(def tmp-queryname-sorted-sam-file-2 (str temp-dir "/" "tmp.queryname.sorted.2.sam"))
(def tmp-queryname-sorted-bam-file-2 (str temp-dir "/" "tmp.queryname.sorted.2.bam"))

(defn- prepare-shuffled-files!
  []
  (let [x (get-shuffled-test-sam)]
    (spit-sam-for-test tmp-shuffled-sam-file x)
    (spit-bam-for-test tmp-shuffled-bam-file x)))

(defn- with-reader
  [target-fn src-file & [dst-file]]
  (let [src-rdr (sam/reader src-file)
        dst-wtr (when dst-file (sam/writer dst-file))]
    (if-not (nil? dst-wtr)
      (with-open [src ^Closeable src-rdr
                  dst ^Closeable dst-wtr] (target-fn src dst))
      (with-open [src ^Closeable src-rdr] (target-fn src)))))

(deftest sort-test-files
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    ;; tests by test-sam-file and test-bam-file
    (is (not-throw? (with-reader sorter/sort-by-pos test-sam-file tmp-coordinate-sorted-sam-file)))
    (is (not-throw? (with-reader sorter/sort-by-pos test-bam-file tmp-coordinate-sorted-bam-file)))
    (is (not-throw? (with-reader sorter/sort-by-qname test-sam-file tmp-queryname-sorted-sam-file)))
    (is (not-throw? (with-reader sorter/sort-by-qname test-bam-file tmp-queryname-sorted-bam-file)))
    (is (not (with-reader sorter/sorted-by? test-sam-file)))
    (is (not (with-reader sorter/sorted-by? test-bam-file)))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-file))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file))
    (is (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file))
    (is (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file))
    (is (= (with-reader sorter/sort-order test-sam-file)
           header/order-unknown))
    (is (= (with-reader sorter/sort-order test-bam-file)
           header/order-unknown))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-sam-file)
           header/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file)
           header/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-sam-file)
           header/order-queryname))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-bam-file)
           header/order-queryname))
    (is (qname-sorted? tmp-queryname-sorted-sam-file))
    (is (qname-sorted? tmp-queryname-sorted-bam-file))))

(deftest about-sorting-shuffled-files
  (with-before-after {:before (do (prepare-cache!)
                                  (prepare-shuffled-files!))
                      :after (clean-cache!)}
    ;; tests by shuffled files (its may sorted by chance)
    (is (not-throw? (with-reader sorter/sort-by-pos tmp-shuffled-sam-file tmp-coordinate-sorted-sam-sam-file)))
    (is (not-throw? (with-reader sorter/sort-by-pos tmp-shuffled-sam-file tmp-coordinate-sorted-sam-bam-file)))
    (is (not-throw? (with-reader sorter/sort-by-pos tmp-shuffled-bam-file tmp-coordinate-sorted-bam-sam-file)))
    (is (not-throw? (with-reader sorter/sort-by-pos tmp-shuffled-bam-file tmp-coordinate-sorted-bam-bam-file)))
    (is (not-throw? (with-reader sorter/sort-by-qname tmp-shuffled-sam-file tmp-queryname-sorted-sam-file-2)))
    (is (not-throw? (with-reader sorter/sort-by-qname tmp-shuffled-bam-file tmp-queryname-sorted-bam-file-2)))
    (is (not (with-reader sorter/sorted-by? tmp-shuffled-sam-file)))
    (is (not (with-reader sorter/sorted-by? tmp-shuffled-bam-file)))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-sam-file))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-bam-file))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-sam-file))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-bam-file))
    (is (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file-2))
    (is (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file-2))
    (is (get #{:queryname :coordinate :unsorted :unknown}
             (with-reader sorter/sort-order tmp-shuffled-sam-file)))
    (is (get #{:queryname :coordinate :unsorted :unknown}
             (with-reader sorter/sort-order tmp-shuffled-bam-file)))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-sam-sam-file)
           header/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-sam-bam-file)
           header/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-sam-file)
           header/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-bam-file)
           header/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-sam-file-2)
           header/order-queryname))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-bam-file-2)
           header/order-queryname))
    ;; check sorting order
    (is (not-throw? (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-sam-sam-file))))
    (is (not-throw? (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-bam-sam-file))))
    (is (not-throw? (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-sam-bam-file))))
    (is (not-throw? (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-bam-file))))
    (is (coord-sorted? tmp-coordinate-sorted-sam-sam-file))
    (is (coord-sorted? tmp-coordinate-sorted-sam-bam-file))
    (is (coord-sorted? tmp-coordinate-sorted-bam-sam-file))
    (is (coord-sorted? tmp-coordinate-sorted-bam-bam-file))
    (is (qname-sorted? tmp-queryname-sorted-sam-file-2))
    (is (qname-sorted? tmp-queryname-sorted-bam-file-2))
    ;; compare generated files
    (is (= (slurp-sam-for-test tmp-coordinate-sorted-sam-sam-file)
           (slurp-bam-for-test tmp-coordinate-sorted-sam-bam-file)
           (slurp-sam-for-test tmp-coordinate-sorted-bam-sam-file)
           (slurp-bam-for-test tmp-coordinate-sorted-bam-bam-file)))
    (is (= (slurp-sam-for-test tmp-queryname-sorted-sam-file-2)
           (slurp-bam-for-test tmp-queryname-sorted-bam-file-2)))))

(deftest about-sorting-small-chunks
  (with-before-after {:before (do (prepare-cache!)
                                  (prepare-shuffled-files!))
                      :after (clean-cache!)}
    ;; coordinate
    (is (not-throw?
         (with-open [r (sam/reader tmp-shuffled-bam-file)
                     w (sam/writer tmp-coordinate-sorted-bam-bam-file)]
           (sorter/sort-by-pos r w {:chunk-size 3}))))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-bam-file))
    (is (not-throw? (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-bam-file))))
    (is (coord-sorted? tmp-coordinate-sorted-bam-bam-file))

    (is (not-throw?
         (with-open [r (sam/reader tmp-shuffled-sam-file)
                     w (sam/writer tmp-coordinate-sorted-sam-sam-file)]
           (sorter/sort-by-pos r w {:chunk-size 3 :cache-fmt :sam}))))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-sam-file))
    (is (not-throw? (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-sam-sam-file))))
    (is (coord-sorted? tmp-coordinate-sorted-sam-sam-file))

    ;; queryname
    (is (not-throw?
         (with-open [r (sam/reader tmp-shuffled-bam-file)
                     w (sam/writer tmp-queryname-sorted-bam-file-2)]
           (sorter/sort-by-qname r w {:chunk-size 3}))))
    (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file-2)
    (is (qname-sorted? tmp-queryname-sorted-bam-file-2))

    (is (not-throw?
         (with-open [r (sam/reader tmp-shuffled-bam-file)
                     w (sam/writer tmp-queryname-sorted-sam-file-2)]
           (sorter/sort-by-qname r w {:chunk-size 3 :cache-fmt :sam}))))
    (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file-2)
    (is (qname-sorted? tmp-queryname-sorted-sam-file-2))))

(deftest-slow about-sorting-medium-file
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (with-reader sorter/sort-by-pos medium-bam-file tmp-coordinate-sorted-sam-file)))
    (is (not-throw? (with-reader sorter/sort-by-pos medium-bam-file tmp-coordinate-sorted-bam-file)))
    (is (not-throw? (with-reader sorter/sort-by-qname medium-bam-file tmp-queryname-sorted-sam-file)))
    (is (not-throw? (with-reader sorter/sort-by-qname medium-bam-file tmp-queryname-sorted-bam-file)))
    (is (not (with-reader sorter/sorted-by? medium-bam-file)))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-file))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file))
    (is (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file))
    (is (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file))
    (is (= (with-reader sorter/sort-order medium-bam-file) header/order-unknown))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-sam-file) header/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file) header/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-sam-file) header/order-queryname))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-bam-file) header/order-queryname))
    ;; check sorting order
    (is (not-throw? (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-sam-file))))
    (is (not-throw? (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-file))))
    (is (coord-sorted? tmp-coordinate-sorted-sam-file))
    (is (coord-sorted? tmp-coordinate-sorted-bam-file))
    (is (qname-sorted? tmp-queryname-sorted-sam-file))
    (is (qname-sorted? tmp-queryname-sorted-bam-file))))

(deftest-remote about-sorting-large-file
  (with-before-after {:before (do (prepare-cavia!)
                                  (prepare-cache!))
                      :after (clean-cache!)}
    (is (not-throw?
         (with-open [r (sam/reader large-bam-file)
                     w (sam/writer tmp-coordinate-sorted-bam-file)]
           ;; Use small chunk size to avoid OOM
           (sorter/sort-by-pos r w {:chunk-size 100000}))))
    (is (not-throw?
         (with-open [r (sam/reader large-bam-file)
                     w (sam/writer tmp-queryname-sorted-bam-file)]
           ;; Use small chunk size to avoid OOM
           (sorter/sort-by-qname r w {:chunk-size 100000}))))
    (is (with-reader sorter/sorted-by? large-bam-file))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-file))
    (is (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file))
    (is (= (with-reader sorter/sort-order large-bam-file) header/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file) header/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-bam-file) header/order-queryname))
    ;; check sorting order
    (is (coord-sorted? tmp-coordinate-sorted-bam-file))
    (is (qname-sorted? tmp-queryname-sorted-bam-file))))

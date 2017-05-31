(ns cljam.t-sorter
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [cljam.core :as core]
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
  (let [src-rdr (core/reader src-file)
        dst-wtr (when dst-file (core/writer dst-file))]
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
           sorter/order-unknown))
    (is (= (with-reader sorter/sort-order test-bam-file)
           sorter/order-unknown))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-sam-file)
           sorter/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-file)
           sorter/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-sam-file)
           sorter/order-queryname))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-bam-file)
           sorter/order-queryname))
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
           sorter/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-sam-bam-file)
           sorter/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-sam-file)
           sorter/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-coordinate-sorted-bam-bam-file)
           sorter/order-coordinate))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-sam-file-2)
           sorter/order-queryname))
    (is (= (with-reader sorter/sort-order tmp-queryname-sorted-bam-file-2)
           sorter/order-queryname))
    ;; check sorting order
    (is (not-throw? (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-sam-sam-file))))
    (is (not-throw? (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-bam-sam-file))))
    (is (not-throw? (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-sam-bam-file))))
    (is (not-throw? (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-bam-file))))
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
         (with-open [r (core/reader tmp-shuffled-bam-file :ignore-index true)
                     w (core/writer tmp-coordinate-sorted-bam-bam-file)]
           (sorter/sort-by-pos r w {:chunk-size 3}))))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-bam-bam-file))
    (is (not-throw? (check-sort-order (slurp-bam-for-test tmp-coordinate-sorted-bam-bam-file))))

    (is (not-throw?
         (with-open [r (core/reader tmp-shuffled-sam-file :ignore-index true)
                     w (core/writer tmp-coordinate-sorted-sam-sam-file)]
           (sorter/sort-by-pos r w {:chunk-size 3 :cache-fmt :sam}))))
    (is (with-reader sorter/sorted-by? tmp-coordinate-sorted-sam-sam-file))
    (is (not-throw? (check-sort-order (slurp-sam-for-test tmp-coordinate-sorted-sam-sam-file))))

    ;; queryname
    (is (not-throw?
         (with-open [r (core/reader tmp-shuffled-bam-file :ignore-index true)
                     w (core/writer tmp-queryname-sorted-bam-file-2)]
           (sorter/sort-by-qname r w {:chunk-size 3}))))
    (with-reader sorter/sorted-by? tmp-queryname-sorted-bam-file-2)
    (is (qname-sorted? tmp-queryname-sorted-bam-file-2))

    (is (not-throw?
         (with-open [r (core/reader tmp-shuffled-bam-file :ignore-index true)
                     w (core/writer tmp-queryname-sorted-sam-file-2)]
           (sorter/sort-by-qname r w {:chunk-size 3 :cache-fmt :sam}))))
    (with-reader sorter/sorted-by? tmp-queryname-sorted-sam-file-2)
    (is (qname-sorted? tmp-queryname-sorted-sam-file-2))))

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

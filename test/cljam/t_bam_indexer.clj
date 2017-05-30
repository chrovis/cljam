(ns cljam.t-bam-indexer
  "Tests for cljam.bam-indexer."
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.t-common :refer :all]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.sorter :as sorter]
            [cljam.bam-indexer :as bai]))

(def temp-file-sorted (str temp-dir "/test.sorted.bam"))
(def temp-file-sorted-2 (str temp-dir "/test.sorted2.bam"))

(deftest about-bam-indexer
  (with-before-after {:before (do (prepare-cache!)
                                  (cio/copy (cio/file test-sorted-bam-file)
                                            (cio/file temp-file-sorted)))
                      :after (clean-cache!)}
    (is (not-throw? (bai/create-index temp-file-sorted
                                      (str temp-file-sorted ".bai"))))
    (is (.isFile (cio/file (str temp-file-sorted ".bai"))))
    (is (same-file? (str temp-file-sorted ".bai") test-bai-file))
    (is (= (with-open [r (bam/reader temp-file-sorted)]
             (doall (io/read-alignments r {:chr "ref" :start 0 :end 1000})))
           (filter #(= "ref" (:rname %))
                   (:alignments test-sam-sorted-by-pos))))))

(deftest about-bam-indexer-invalid-files
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (thrown? Exception (bai/create-index "not-exists-file"
                                             "not-exists-file.bai")))))

(deftest about-bam-indexer-for-incomplete-alignments
  (let [f (str temp-dir "/test.incomplete.bam")
        sorted-f (str temp-dir "/test.incomplete.sorted.bam")]
    (with-before-after {:before (do (prepare-cache!)
                                    ;; generate incomplete bam on the fly
                                    (spit-bam-for-test f test-sam-incomplete-alignments)
                                    ;; TODO: go independent from sorter
                                    (with-open [rdr (bam/reader f :ignore-index true)
                                                wtr (bam/writer sorted-f)]
                                      (sorter/sort-by-pos rdr wtr)))
                        :after (clean-cache!)}
      (is (not-throw? (bai/create-index sorted-f (str sorted-f ".bai"))))
      (is (.isFile (cio/file (str sorted-f ".bai"))))
      (is (= (with-open [r (bam/reader sorted-f)]
               (doall (io/read-alignments r {:chr "ref" :start 0 :end 1000})))
             (filter #(= "ref" (:rname %))
                     (:alignments test-sam-incomplete-alignments-sorted-by-pos))))
      ;; TODO: need more strictly check to .bai files
      ;; (it will use https://gitlab.xcoo.jp/chrovis/cljam/issues/8 later)
      )))

(deftest about-bam-indexer-small-file
  (with-before-after {:before (do (prepare-cache!)
                                  (cio/copy (cio/file small-bam-file)
                                            (cio/file temp-file-sorted))
                                  (cio/copy (cio/file small-bam-file)
                                            (cio/file temp-file-sorted-2)))
                      :after (clean-cache!)}
    (let [temp-file-sorted-bai (str temp-file-sorted ".bai")
          temp-file-sorted-bai-2 (str temp-file-sorted-2 ".bai")]
      (is (not-throw? (bai/create-index temp-file-sorted
                                        temp-file-sorted-bai
                                        :n-threads 1)))
      (is (.isFile (cio/file temp-file-sorted-bai)))
      (is (not-throw? (bai/create-index temp-file-sorted-2
                                        temp-file-sorted-bai-2
                                        :n-threads 4)))
      (is (.isFile (cio/file temp-file-sorted-bai-2)))
      (is (same-file? temp-file-sorted-bai temp-file-sorted-bai-2))
      (with-open [r (bam/reader temp-file-sorted)]
        ;; Random read with different number of spans.
        (are [?param ?counts] (= (count (io/read-alignments r ?param)) ?counts)
          {:chr "chr1" :start 23000000 :end 23001000 :depth :deep} 46 ;; 1 span
          {:chr "chr1" :start 24900000 :end 24902000 :depth :deep} 3  ;; 2 spans
          {:chr "chr1" :start 24000000 :end 24001000 :depth :deep} 6  ;; 3 spans
          {:chr "chr1" :start 23260000 :end 23268650 :depth :deep} 58 ;; 4 spans
          {:chr "chr1" :start 23430000 :end 23470000 :depth :deep} 55 ;; 5 spans
          {:chr "*"} 0)))))

(deftest-slow about-bam-indexer-medium-file
  (with-before-after {:before (do (prepare-cache!)
                                  (cio/copy (cio/file medium-bam-file)
                                            (cio/file temp-file-sorted)))
                      :after (clean-cache!)}
    (is (not-throw? (bai/create-index temp-file-sorted
                                      (str temp-file-sorted ".bai"))))
    (with-open [r (bam/reader temp-file-sorted)]
      (is (= (count (io/read-alignments r {:chr "*"})) 4348))
      (is (every? (fn [a] (and (= (:mapq a) 0)
                               (= (:pos a) 0)
                               (= (:tlen a) 0)
                               (pos? (bit-and (:flag a) 4))))
                  (io/read-alignments r {:chr "*"}))))
    (is (.isFile (cio/file (str temp-file-sorted ".bai"))))))

(deftest-remote about-bam-indexer-large-file
  (with-before-after {:before (do (prepare-cavia!)
                                  (prepare-cache!)
                                  (cio/copy (cio/file large-bam-file)
                                            (cio/file temp-file-sorted)))
                      :after (clean-cache!)}
    (is (not-throw? (bai/create-index temp-file-sorted
                                      (str temp-file-sorted ".bai"))))
    (is (.isFile (cio/file (str temp-file-sorted ".bai"))))))

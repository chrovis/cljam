(ns cljam.t-bam-indexer
  "Tests for cljam.bam-indexer."
  (:require [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [cljam.t-common :refer :all]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.sorter :as sorter]
            [cljam.bam-indexer :as bai]))

(def temp-file-sorted (str temp-dir "/test.sorted.bam"))

(deftest about-bam-indexer
  (with-before-after {:before (do (prepare-cache!)
                                  (fs/copy test-sorted-bam-file
                                           temp-file-sorted))
                      :after (clean-cache!)}
    (is (not-throw? (bai/create-index temp-file-sorted
                                      (str temp-file-sorted ".bai"))))
    (is (fs/exists? (str temp-file-sorted ".bai")))
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
      (is (fs/exists? (str sorted-f ".bai")))
      (is (= (with-open [r (bam/reader sorted-f)]
               (doall (io/read-alignments r {:chr "ref" :start 0 :end 1000})))
             (filter #(= "ref" (:rname %))
                     (:alignments test-sam-incomplete-alignments-sorted-by-pos))))
      ;; TODO: need more strictly check to .bai files
      ;; (it will use https://gitlab.xcoo.jp/chrovis/cljam/issues/8 later)
      )))

(deftest about-bam-indexer-small-file
  (with-before-after {:before (do (prepare-cache!)
                                  (fs/copy small-bam-file temp-file-sorted))
                      :after (clean-cache!)}
    (is (not-throw? (bai/create-index temp-file-sorted
                                      (str temp-file-sorted ".bai"))))
    (is (fs/exists? (str temp-file-sorted ".bai")))
    (with-open [r (bam/reader temp-file-sorted)]
      ;; Random read with different number of spans.
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 25000000 :depth :shallow}))
             14858))
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 25000000 :depth :pointer}))
             14858))
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 25000000 :depth :deep}))
             14858))
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 24500000 :depth :deep}))
             11424))
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 24000000 :depth :deep}))
             10010))
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 23500000 :depth :deep}))
             3806))
      (is (= (count (io/read-alignments r {:chr "*"})) 0)))))

(deftest about-bam-indexer-small-file-with-n-threads-1
  (with-before-after {:before (do (prepare-cache!)
                                  (fs/copy small-bam-file temp-file-sorted))
                      :after (clean-cache!)}
    (is (not-throw? (bai/create-index temp-file-sorted
                                      (str temp-file-sorted ".bai")
                                      :n-threads 1)))
    (is (fs/exists? (str temp-file-sorted ".bai")))
    (with-open [r (bam/reader temp-file-sorted)]
      ;; Random read with different number of spans.
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 25000000 :depth :shallow}))
             14858))
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 25000000 :depth :pointer}))
             14858))
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 25000000 :depth :deep}))
             14858))
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 24500000 :depth :deep}))
             11424))
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 24000000 :depth :deep}))
             10010))
      (is (= (count (io/read-alignments r {:chr "chr1" :start 23000000 :end 23500000 :depth :deep}))
             3806))
      (is (= (count (io/read-alignments r {:chr "*"})) 0)))))

(deftest-slow about-bam-indexer-medium-file
  (with-before-after {:before (do (prepare-cache!)
                                  (fs/copy medium-bam-file temp-file-sorted))
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
    (is (fs/exists? (str temp-file-sorted ".bai")))))

(deftest-slow-heavy about-bam-indexer-large-file
  (with-before-after {:before (do (prepare-cavia!)
                                  (prepare-cache!)
                                  (fs/copy large-bam-file temp-file-sorted))
                      :after (clean-cache!)}
    (is (not-throw? (bai/create-index temp-file-sorted
                                      (str temp-file-sorted ".bai"))))
    (is (fs/exists? (str temp-file-sorted ".bai")))))

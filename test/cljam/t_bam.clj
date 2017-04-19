(ns cljam.t-bam
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [clojure.java.io :refer [copy file delete-file]]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.bam-index.core :as bai-core]
            [cljam.sorter :as sorter])
  (:import [java.io IOException]))

(def temp-file (str temp-dir "/test.bam"))
(def temp-file-sorted (str temp-dir "/test.sorted.bam"))
(def not-found-file (str temp-dir "/not-found.bam"))
(def invalid-file-1 test-fa-file)
(def invalid-file-2 test-tabix-file)

(deftest slurp-bam
  (is (= (slurp-bam-for-test test-bam-file) test-sam)))

(deftest-slow slurp-bam-medium-file
  (is (not-throw? (slurp-bam-for-test medium-bam-file))))

;; NB: Cannot slurp large BAM (cause `java.lang.OutOfMemoryError`)
;; (deftest-slow-heavy slurp-bam-large-file
;;   (with-before-after {:before (prepare-cavia!)}
;;     (is (not-throw? (slurp-bam-for-test large-bam-file)))))

(deftest spit-bam
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-bam-for-test temp-file test-sam)))
    (is (= (slurp-bam-for-test temp-file) test-sam))))

(deftest-slow spit-bam-medium-file
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-bam-for-test temp-file
                                       (slurp-bam-for-test medium-bam-file))))))

;; NB: Cannot spit large BAM (cause `java.lang.OutOfMemoryError`)
;; (deftest-slow-heavy spit-bam-large-file
;;   (with-before-after {:before (do (prepare-cavia!)
;;                                   (prepare-cache!))
;;                       :after (clean-cache!)}
;;     (is (not-throw? (spit-bam-for-test temp-file
;;                                        (slurp-bam-for-test large-bam-file))))))

(defn- shallow= [alns1 alns2]
  (when (= (count alns1) (count alns2))
    (loop [alns1 alns1
           alns2 alns2]
      (let [aln1 (first alns1)
            aln2 (first alns2)]
        (if (nil? aln1)
          true
          (when (and (= (:rname aln1) (:rname aln2))
                     (= (:pos aln1) (:pos aln2)))
            (recur (rest alns1) (rest alns2))))))))

(defn- pointer= [alns1 alns2]
  (when (= (count alns1) (count alns2))
    (loop [alns1 alns1
           alns2 alns2]
      (let [aln1 (first alns1)
            aln2 (first alns2)]
        (if (nil? aln1)
          true
          (when (and (= (:rname aln1) (:rname aln2))
                     (= (:pos aln1) (:pos aln2))
                     (= (:flag aln1) (:flag aln2)))
            (recur (rest alns1) (rest alns2))))))))

(deftest bamreader
  (with-before-after {:before (do (prepare-cache!)
                                  (spit-bam-for-test temp-file test-sam))
                      :after (clean-cache!)}
    (let [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (= (io/read-alignments rdr) (:alignments test-sam))))
    (let [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (thrown? Exception (io/read-alignments rdr {:chr "ref2"}))))
    (let [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (= (io/read-alignments rdr {:depth :deep}) (:alignments test-sam))))
    (let [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (shallow= (io/read-alignments rdr {:depth :shallow})
                    (:alignments test-sam))))
    (let [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (pointer= (io/read-alignments rdr {:depth :pointer})
                    (:alignments test-sam))))
    (let [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (= (data->clj (io/read-blocks rdr)) test-sam-data)))
    (let [rdr (bam/reader temp-file :ignore-index false)]
      (is (= (io/read-refs rdr) test-sam-refs))
      (is (thrown? Exception (data->clj (io/read-blocks rdr {:chr "ref2"})))))))

(deftest bamreader-with-index
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      ;; TODO: Does not works?
      (is (= (io/read-alignments rdr {:chr "ref2"}) [])))
    (let [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      (is (= (data->clj (io/read-blocks rdr)) test-sorted-bam-data)))
    (let [rdr (bam/reader test-sorted-bam-file :ignore-index false)]
      ;; TODO: Does not works?
      (is (= (data->clj (io/read-blocks rdr {:chr "ref2"})) [])))))

(deftest bamreader-invalid-files
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (thrown? Exception (bam/reader invalid-file-1 :ignore-index true)))
    (is (thrown? IOException (bam/reader invalid-file-2 :ignore-index true)))
    (is (thrown? IOException (bam/reader not-found-file :ignore-index true)))))

(deftest bamreader-medium-file
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (with-open [rdr (bam/reader medium-bam-file :ignore-index true)]
      (let [header (io/read-header rdr)
            refs (io/read-refs rdr)
            alns (io/read-alignments rdr)]
        (is (= refs medium-sam-refs))
        (with-open [w (bam/writer temp-file)]
          (is (not-throw? (io/write-header w header)))
          (is (not-throw? (io/write-refs w refs)))
          (is (not-throw? (io/write-alignments w alns header)))
          (same-file? medium-bam-file temp-file))))))

(deftest-slow-heavy bamreader-large-file
  (with-before-after {:before (do (prepare-cache!)
                                  (prepare-cavia!))
                      :after (clean-cache!)}
    (let [rdr (bam/reader large-bam-file :ignore-index true)]
      (is (= (io/read-refs rdr) large-sam-refs))
      (is (not-throw? (io/read-alignments rdr))))))

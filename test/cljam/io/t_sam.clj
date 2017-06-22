(ns cljam.io.t-sam
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.t-common :refer :all]
            [cljam.io.sam :as sam]
            [cljam.util :as util]))

(def temp-sam-file (str temp-dir "/test.sam"))
(def temp-bam-file (str temp-dir "/test.bam"))
(def temp-bam-file-sorted (str temp-dir "/test.sorted.bam"))
(def not-found-bam-file (str temp-dir "/not-found.bam"))
(def invalid-bam-file-1 test-fa-file)
(def invalid-bam-file-2 test-tabix-file)

(deftest slurp-sam-test
  (is (= (slurp-sam-for-test test-sam-file) test-sam)))

(deftest slurp-bam-test
  (is (= (slurp-bam-for-test test-bam-file) test-sam)))

(deftest-slow slurp-bam-medium-test
  (is (not-throw? (slurp-bam-for-test medium-bam-file))))

(deftest spit-sam-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-sam-for-test temp-sam-file test-sam)))
    (is (= (slurp-sam-for-test temp-sam-file) test-sam))))

(deftest-slow spit-sam-medium-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-sam-for-test temp-sam-file
                                       (slurp-bam-for-test medium-bam-file))))))

;; NB: Cannot spit large SAM (cause `java.lang.OutOfMemoryError`)
;; (deftest-slow-heavy about-spit-sam-large-file
;;   (with-before-after {:before (do (prepare-cavia!)
;;                                   (prepare-cache!))
;;                       :after (clean-cache!)}
;;     (is (not-throw? (spit-sam-for-test temp-sam-file
;;                                        (slurp-bam-for-test large-bam-file))))))

(deftest spit-bam-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-bam-for-test temp-bam-file test-sam)))
    (is (= (slurp-bam-for-test temp-bam-file) test-sam))))

(deftest-slow spit-bam-medium-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-bam-for-test temp-bam-file
                                       (slurp-bam-for-test medium-bam-file))))))

;; NB: Cannot spit large BAM (cause `java.lang.OutOfMemoryError`)
;; (deftest-slow-heavy spit-bam-large-file
;;   (with-before-after {:before (do (prepare-cavia!)
;;                                   (prepare-cache!))
;;                       :after (clean-cache!)}
;;     (is (not-throw? (spit-bam-for-test temp-bam-file
;;                                        (slurp-bam-for-test large-bam-file))))))

(defn- shallow= [alns1 alns2]
  (= (map #(select-keys % [:rname :pos]) alns1)
     (map #(select-keys % [:rname :pos]) alns2)))

(defn- pointer= [alns1 alns2]
  (= (map #(select-keys % [:rname :pos :flag]) alns1)
     (map #(select-keys % [:rname :pos :flag]) alns2)))

(deftest sam-reader-test
  (with-before-after {:before (do (prepare-cache!)
                                  (spit-sam-for-test temp-sam-file test-sam))
                      :after (clean-cache!)}
    (with-open [rdr (sam/sam-reader temp-sam-file)]
      (is (= (sam/read-refs rdr) test-sam-refs)))))

(deftest bam-reader-test
  (with-before-after {:before (do (prepare-cache!)
                                  (spit-bam-for-test temp-bam-file test-sam))
                      :after (clean-cache!)}
    (with-open [rdr (sam/bam-reader temp-bam-file :ignore-index false)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (= (sam/read-alignments rdr) (:alignments test-sam))))
    (with-open [rdr (sam/bam-reader temp-bam-file :ignore-index false)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (thrown? Exception (sam/read-alignments rdr {:chr "ref2"}))))
    (with-open [rdr (sam/bam-reader temp-bam-file :ignore-index false)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (= (sam/read-alignments rdr {:depth :deep}) (:alignments test-sam))))
    (with-open [rdr (sam/bam-reader temp-bam-file :ignore-index false)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (shallow= (sam/read-alignments rdr {:depth :shallow})
                    (:alignments test-sam))))
    (with-open [rdr (sam/bam-reader temp-bam-file :ignore-index false)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (pointer= (sam/read-alignments rdr {:depth :pointer})
                    (:alignments test-sam))))
    (with-open [rdr (sam/bam-reader temp-bam-file :ignore-index false)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (= (data->clj (sam/read-blocks rdr)) test-sam-data)))
    (with-open [rdr (sam/bam-reader temp-bam-file :ignore-index false)]
      (is (= (sam/read-refs rdr) test-sam-refs))
      (is (thrown? Exception (data->clj (sam/read-blocks rdr {:chr "ref2"})))))))

(deftest bam-reader-with-index-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (with-open [rdr (sam/bam-reader test-sorted-bam-file :ignore-index false)]
      (is (= (sam/read-alignments rdr {:chr "ref2"})
             (drop 6 (:alignments test-sam-sorted-by-pos)))))
    (with-open [rdr (sam/bam-reader test-sorted-bam-file :ignore-index false)]
      (is (= (sam/read-alignments rdr {:chr "ref2" :start 21})
             (drop 7 (:alignments test-sam-sorted-by-pos)))))
    (with-open [rdr (sam/bam-reader test-sorted-bam-file :ignore-index false)]
      (is (= (sam/read-alignments rdr {:chr "ref2" :end 9})
             (take 3 (drop 6 (:alignments test-sam-sorted-by-pos))))))
    (with-open [rdr (sam/bam-reader test-sorted-bam-file :ignore-index false)]
      (is (= (sam/read-alignments rdr {:chr "ref2" :start 10 :end 12})
             (take 5 (drop 6 (:alignments test-sam-sorted-by-pos))))))
    (with-open [rdr (sam/bam-reader test-sorted-bam-file :ignore-index false)]
      (is (= (data->clj (sam/read-blocks rdr))
             test-sorted-bam-data)))
    (with-open [rdr (sam/bam-reader test-sorted-bam-file :ignore-index false)]
      (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                  (data->clj (sam/read-blocks rdr {:chr "ref2"})))
             (drop 6 test-sorted-bam-data))))
    (with-open [rdr (sam/bam-reader test-sorted-bam-file :ignore-index false)]
      (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                  (data->clj (sam/read-blocks rdr {:chr "ref2" :start 2})))
             (drop 7 test-sorted-bam-data))))
    (with-open [rdr (sam/bam-reader test-sorted-bam-file :ignore-index false)]
      (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                  (data->clj (sam/read-blocks rdr {:chr "ref2" :end 2})))
             (take 2 (drop 6 test-sorted-bam-data)))))
    (with-open [rdr (sam/bam-reader test-sorted-bam-file :ignore-index false)]
      (is (= (map #(dissoc % :pos :qname :rname :flag :ref-id)
                  (data->clj (sam/read-blocks rdr {:chr "ref2" :start 4 :end 12})))
             (take 3 (drop 8 test-sorted-bam-data)))))))

(deftest bam-reader-invalid-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (thrown? Exception (sam/bam-reader invalid-bam-file-1 :ignore-index true)))
    (is (thrown? java.io.IOException (sam/bam-reader invalid-bam-file-2 :ignore-index true)))
    (is (thrown? java.io.IOException (sam/bam-reader not-found-bam-file :ignore-index true)))))

(deftest-slow bam-reader-medium-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (with-open [rdr (sam/bam-reader medium-bam-file :ignore-index true)]
      (let [header (sam/read-header rdr)
            refs (sam/read-refs rdr)
            alns (sam/read-alignments rdr)]
        (is (= refs medium-sam-refs))
        (with-open [w (sam/bam-writer temp-bam-file)]
          (is (not-throw? (sam/write-header w header)))
          (is (not-throw? (sam/write-refs w refs)))
          (is (not-throw? (sam/write-alignments w alns header)))
          (same-file? medium-bam-file temp-bam-file))))))

(deftest-remote bam-reader-large-test
  (with-before-after {:before (do (prepare-cache!)
                                  (prepare-cavia!))
                      :after (clean-cache!)}
    (with-open [rdr (sam/bam-reader large-bam-file :ignore-index true)]
      (is (= (sam/read-refs rdr) large-sam-refs))
      (is (not-throw? (sam/read-alignments rdr))))))

(deftest reader-test
  (testing "sam"
    (with-open [rdr (sam/reader test-sam-file)]
      (is (instance? cljam.io.sam.reader.SAMReader rdr))))
  (testing "bam"
    (with-open [rdr (sam/reader test-bam-file)]
      (is (instance? cljam.io.bam.reader.BAMReader rdr))))
  (testing "clone bam"
    (with-open [rdr (sam/reader test-bam-file)
                crdr (sam/reader rdr)]
      (is (instance? cljam.io.bam.reader.BAMReader crdr))))
  (testing "throws Exception"
    (are [f] (thrown? Exception (sam/reader f))
      "./test-resources/bam/foo.bam"
      test-bai-file
      "./test-resources/bam/foo.baam")))

(deftest sam-writer-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (with-open [w (sam/sam-writer temp-sam-file)]
      (is (not-throw? (sam/write-header w (:header test-sam))))
      (is (not-throw? (sam/write-alignments w (:alignments test-sam) nil))))
    (with-open [r (sam/sam-reader temp-sam-file)]
      (is (= (sam/read-header r) (:header test-sam)))
      (is (= (sam/read-refs r) test-sam-refs))
      (is (= (sam/read-blocks r) test-sam-blocks)))
    (with-open [w (sam/sam-writer temp-sam-file)]
      (is (not-throw? (sam/write-header w (:header test-sam))))
      (is (not-throw? (sam/write-blocks w test-sam-blocks))))
    (with-open [r (sam/sam-reader temp-sam-file)]
      (is (= (sam/read-header r) (:header test-sam)))
      (is (= (sam/read-refs r) test-sam-refs))
      (is (= (sam/read-alignments r) (:alignments test-sam))))))

(deftest writer-test
  (testing "sam"
    (with-open [wtr (sam/writer (.getAbsolutePath (cio/file util/temp-dir "temp.sam")))]
      (is (instance? cljam.io.sam.writer.SAMWriter wtr))))
  (testing "bam"
    (with-open [wtr (sam/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bam")))]
      (is (instance? cljam.io.bam.writer.BAMWriter wtr))))
  (testing "throws Exception"
    (are [f] (thrown? Exception (sam/writer (.getAbsolutePath (cio/file util/temp-dir f))))
      "temp.baam"
      "temp.bai")))

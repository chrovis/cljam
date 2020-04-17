(ns cljam.algo.depth-test
  (:require [clojure.test :refer :all]
            [cljam.test-common :as common]
            [cljam.io.sam :as sam]
            [cljam.algo.depth :as depth])
  (:import [clojure.lang LazySeq ArraySeq]))

(def test-bam-depth-ref
  [0 0 0 0 0 0 1 1 3 3 3 3 3 3 2 3 3 3 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 1 1 1 2 2 2 2 1 1 1 1 1])
(def test-bam-depth-ref2
  [1 2 2 2 2 3 3 3 3 4 4 5 5 6 6 6 6 6 6 6 5 5 4 4 4 4 4 3 3 3 3 3 3 3 2 1 0 0 0 0])

(deftest depth-laziness
  (testing "lazy-depth returns a LazySeq"
    (with-open [r (sam/reader common/test-sorted-bam-file)]
      (is (instance? LazySeq (depth/lazy-depth r {:chr "ref"}))))
    (with-open [r (sam/reader common/test-sorted-bam-file)]
      (is (instance? LazySeq (depth/lazy-depth r {:chr "ref"} {:step 3})))))
  (testing "depth returns a seq"
    (with-open [r (sam/reader common/test-sorted-bam-file)]
      (is (not (instance? LazySeq (depth/depth r {:chr "ref"})))))
    (with-open [r (sam/reader common/test-sorted-bam-file)]
      (is (not (instance? LazySeq (depth/depth r {:chr "ref"} {:step 3})))))))

(deftest depth-range-and-length
  (testing "in"
    (is (= 10
           (with-open [r (sam/reader common/test-sorted-bam-file)]
             (count (depth/lazy-depth r {:chr "ref", :start 1, :end 10})))))
    (is (= 10
           (with-open [r (sam/reader common/test-sorted-bam-file)]
             (count (depth/depth r {:chr "ref", :start 1, :end 10}))))))
  (testing "over"
    (is (= 45
           (with-open [r (sam/reader common/test-sorted-bam-file)]
             (count (depth/lazy-depth r {:chr "ref", :start 1, :end 100})))))
    (is (= 45
           (with-open [r (sam/reader common/test-sorted-bam-file)]
             (count (depth/depth r {:chr "ref", :start 1, :end 100}))))))
  (testing "under"
    (is (thrown? AssertionError
                 (with-open [r (sam/reader common/test-sorted-bam-file)]
                   (count (depth/lazy-depth r {:chr "ref", :start 0, :end 45})))))
    (is (thrown? AssertionError
                 (with-open [r (sam/reader common/test-sorted-bam-file)]
                   (count (depth/depth r {:chr "ref", :start 0, :end 45}))))))
  (testing "invalid"
    (is (thrown? AssertionError
                 (with-open [r (sam/reader common/test-sorted-bam-file)]
                   (count (depth/lazy-depth r {:chr "ref", :start 10, :end 5})))))
    (is (thrown? AssertionError
                 (with-open [r (sam/reader common/test-sorted-bam-file)]
                   (count (depth/depth r {:chr "ref", :start 10, :end 5})))))))

(deftest lazy-depth-returns-same-results
  (is (= (with-open [r (sam/reader common/test-sorted-bam-file)]
           (doall (depth/lazy-depth r {:chr "ref"})))
         test-bam-depth-ref))
  (is (= (with-open [r (sam/reader common/test-sorted-bam-file)]
           (doall (depth/lazy-depth r {:chr "ref2"})))
         test-bam-depth-ref2))
  (is (= (with-open [r (sam/reader common/test-sorted-bam-file)]
           (doall (depth/depth r {:chr "ref"})))
         test-bam-depth-ref))
  (is (= (with-open [r (sam/reader common/test-sorted-bam-file)]
           (doall (depth/depth r {:chr "ref2"})))
         test-bam-depth-ref2)))

(deftest depth-values-option
  (are [?unchecked]
       (are [?n-threads]
            (are [?step]
                 (are [?fn]
                      (= (with-open [r (sam/bam-reader common/test-sorted-bam-file)]
                           (doall (?fn r {:chr "ref"} {:step ?step, :n-threads ?n-threads, :unchecked? ?unchecked})))
                         test-bam-depth-ref)
                   depth/depth
                   depth/lazy-depth)
              2 3 5 7 11 13)
         1 2 3 4)
    true false))

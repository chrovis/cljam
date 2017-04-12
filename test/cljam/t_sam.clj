(ns cljam.t-sam
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.io :as io]))

(def temp-file (str temp-dir "/test.sam"))

(deftest about-slurp-sam
  (is (= (slurp-sam-for-test test-sam-file) test-sam)))

(deftest about-spit-sam
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-sam-for-test temp-file test-sam)))
    (is (= (slurp-sam-for-test temp-file) test-sam))))

(deftest-slow about-spit-sam-medium-file
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-sam-for-test temp-file
                                       (slurp-bam-for-test medium-bam-file))))))

;; NB: Cannot spit large SAM (cause `java.lang.OutOfMemoryError`)
;; (deftest-slow-heavy about-spit-sam-large-file
;;   (with-before-after {:before (do (prepare-cavia!)
;;                                   (prepare-cache!))
;;                       :after (clean-cache!)}
;;     (is (not-throw? (spit-sam-for-test temp-file
;;                                        (slurp-bam-for-test large-bam-file))))))

(deftest about-samreader
  (with-before-after {:before (do (prepare-cache!)
                                  (spit-sam-for-test temp-file test-sam))
                      :after (clean-cache!)}
    (with-open [rdr (sam/reader temp-file)]
      (is (= (io/read-refs rdr) test-sam-refs)))))

(deftest about-samwriter
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (with-open [w (sam/writer temp-file)]
      (is (not-throw? (io/write-header w (:header test-sam))))
      (is (not-throw? (io/write-alignments w (:alignments test-sam) nil))))
    (with-open [r (sam/reader temp-file)]
      (is (= (io/read-header r) (:header test-sam)))
      (is (= (io/read-refs r) test-sam-refs))
      (is (= (io/read-blocks r) test-sam-blocks)))
    (with-open [w (sam/writer temp-file)]
      (is (not-throw? (io/write-header w (:header test-sam))))
      (is (not-throw? (io/write-blocks w test-sam-blocks))))
    (with-open [r (sam/reader temp-file)]
      (is (= (io/read-header r) (:header test-sam)))
      (is (= (io/read-refs r) test-sam-refs))
      (is (= (io/read-alignments r) (:alignments test-sam))))))

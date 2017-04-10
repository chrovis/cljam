(ns cljam.t-bam
  (:require [clojure.test :refer :all]
            [cljam.t-common :refer :all]
            [clojure.java.io :refer [copy file]]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.sorter :as sorter]))

(def temp-file (str temp-dir "/test.bam"))
(def temp-file-sorted (str temp-dir "/test.sorted.bam"))

(deftest slurp-bam
  (is (= (slurp-bam-for-test test-bam-file) test-sam)))

(deftest ^:slow slurp-bam-medium-file
  (is (not-throw? (slurp-bam-for-test medium-bam-file))))

;; NB: Cannot slurp large BAM (cause `java.lang.OutOfMemoryError`)
;; (deftest ^:slow ^:heavy slurp-bam-large-file
;;   (with-before-after {:before (prepare-cavia!)}
;;     (is (not-throw? (slurp-bam-for-test large-bam-file)))))

(deftest spit-bam
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-bam-for-test temp-file test-sam)))
    (is (= (slurp-bam-for-test temp-file) test-sam))))

(deftest ^:slow spit-bam-medium-file
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (is (not-throw? (spit-bam-for-test temp-file
                                       (slurp-bam-for-test medium-bam-file))))))

;; NB: Cannot spit large BAM (cause `java.lang.OutOfMemoryError`)
;; (deftest ^:slow ^:heavy spit-bam-large-file
;;   (with-before-after {:before (do (prepare-cavia!)
;;                                   (prepare-cache!))
;;                       :after (clean-cache!)}
;;     (is (not-throw? (spit-bam-for-test temp-file
;;                                        (slurp-bam-for-test large-bam-file))))))

(deftest bamreader
  (with-before-after {:before (do (prepare-cache!)
                                  (spit-bam-for-test temp-file test-sam))
                      :after (clean-cache!)}
    (let [rdr (bam/reader temp-file :ignore-index true)]
      (is (= (io/read-refs rdr) test-sam-refs)))))

(deftest ^:slow bamreader-medium-file
  (with-before-after {:before (do (prepare-cache!)
                                  (spit-bam-for-test temp-file test-sam))
                      :after (clean-cache!)})
    (let [rdr (bam/reader medium-bam-file :ignore-index true)]
      (is (= (io/read-refs rdr) medium-sam-refs))))

(deftest ^:slow ^:heavy bamreader-large-file
  (with-before-after {:before (do (prepare-cache!)
                                  (prepare-cavia!))
                      :after (clean-cache!)}
    (let [rdr (bam/reader large-bam-file :ignore-index true)]
      (is (= (io/read-refs rdr) large-sam-refs)))))

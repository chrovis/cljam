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
    (let [rdr (sam/reader temp-file)]
      (is (= (io/read-refs rdr) test-sam-refs)))))

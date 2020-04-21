(ns cljam.algo.normal-test
  (:require [clojure.test :refer [deftest is testing]]
            [cljam.test-common :refer
             [with-before-after
              prepare-cache!
              clean-cache!
              not-throw?
              same-sam-contents?
              temp-dir
              normalize-before-sam-file
              normalize-after-sam-file
              normalize-before-bam-file
              normalize-after-bam-file]]
            [cljam.algo.normal :as normal]
            [cljam.io.sam :as sam]))

(def temp-sam (str temp-dir "/out.sam"))
(def temp-bam (str temp-dir "/out.bam"))

(deftest normalize-test
  (testing "sam -> sam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [rdr (sam/reader normalize-before-sam-file)
                  wtr (sam/writer temp-sam)]
        (is (not-throw? (normal/normalize rdr wtr))))
      (is (same-sam-contents? temp-sam normalize-after-sam-file))))
  (testing "bam -> bam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [rdr (sam/reader normalize-before-bam-file)
                  wtr (sam/writer temp-bam)]
        (is (not-throw? (normal/normalize rdr wtr))))
      (is (same-sam-contents? temp-bam normalize-after-bam-file))))
  (testing "sam -> bam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [rdr (sam/reader normalize-before-sam-file)
                  wtr (sam/writer temp-bam)]
        (is (not-throw? (normal/normalize rdr wtr))))
      (is (same-sam-contents? temp-bam normalize-after-bam-file))))
  (testing "bam -> sam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (with-open [rdr (sam/reader normalize-before-bam-file)
                  wtr (sam/writer temp-sam)]
        (is (not-throw? (normal/normalize rdr wtr))))
      (is (same-sam-contents? temp-sam normalize-after-sam-file)))))

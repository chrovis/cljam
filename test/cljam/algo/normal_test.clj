(ns cljam.algo.normal-test
  (:require [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.algo.normal :refer :all]
            [cljam.io.sam :as sam]))

(def temp-sam (str temp-dir "/out.sam"))
(def temp-bam (str temp-dir "/out.bam"))

(deftest normalize-test
  (testing "sam -> sam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (is (not-throw? (normalize-file! normalize-before-sam-file temp-sam)))
      (is (same-sam-contents? temp-sam normalize-after-sam-file))))
  (testing "bam -> bam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (is (not-throw? (normalize-file! normalize-before-bam-file temp-bam)))
      (is (same-sam-contents? temp-bam normalize-after-bam-file))))
  (testing "sam -> bam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (is (not-throw? (normalize-file! normalize-before-sam-file temp-bam)))
      (is (same-sam-contents? temp-bam normalize-after-bam-file))))
  (testing "bam -> sam"
    (with-before-after {:before (prepare-cache!)
                        :after (clean-cache!)}
      (is (not-throw? (normalize-file! normalize-before-bam-file temp-sam)))
      (is (same-sam-contents? temp-sam normalize-after-sam-file)))))

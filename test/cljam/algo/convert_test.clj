(ns cljam.algo.convert-test
  (:require [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.algo.convert :as convert]))

(deftest convert-test
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (testing "SAM -> BAM"
      (let [in test-sam-file
            out (str temp-dir "/test.bam")]
        (is (not-throw? (convert/convert in out)))
        (is (same-sam-contents? in out))))
    (testing "BAM -> SAM"
      (let [in test-bam-file
            out (str temp-dir "/test.sam")]
        (is (not-throw? (convert/convert in out)))
        (is (same-sam-contents? in out))))
    (testing "FASTA -> TwoBit"
      (let [in test-fa-file
            out (str temp-dir "/test.2bit")]
        (is (not-throw? (convert/convert in out)))
        (is (same-sequence-contents? in out))))
    (testing "TwoBit -> FASTA"
      (let [in test-twobit-file
            out (str temp-dir "/test.fa")]
        (is (not-throw? (convert/convert in out)))
        (is (same-sequence-contents? in out))))
    (testing "error"
      (are [in out] (thrown? Exception (convert/convert in out))
        test-bam-file (str temp-dir "/test.unknown")
        test-bam-file (str temp-dir "/test.2bit")))))

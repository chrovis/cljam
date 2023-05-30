(ns cljam.io.bam.decoder-test
  "Tests for cljam.io.bam.decoder."
  (:require [clojure.test :refer [deftest is are testing]]
            [cljam.io.bam.decoder :as decoder]
            [cljam.test-common :as test-common]))

(deftest B-I-type-cigar-str->cigar-str-test
  (is (= "1M2I4D8N16S32H64P128=65535X"
         (#'decoder/B-I-type-cigar-str->cigar-str
          "I,16,33,66,131,260,517,1030,2055,1048568"))))

(deftest decode-alignment-test
  (let [rid-bin [0 0 0 0 1 0 0 0 2 0 73 18]
        flag-qname [0 0 2 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 81 0]
        seq-qual [17 -1 -1]
        no-opts (concat rid-bin
                        [1 0]        ;; n_cigar_op: 1
                        flag-qname
                        [32 0 0 0]   ;; cigar: 2M
                        seq-qual)
        with-CG (concat rid-bin
                        [2 0]        ;; n_cigar_op: 2
                        flag-qname
                        [36 0 0 0    ;; cigar: 2S
                         35 0 0 0]   ;; 2N
                        seq-qual
                        [67 71 66 73 ;; tag: CG, val_type: B-I
                         1 0 0 0     ;; count
                         32 0 0 0])  ;; cigar: 2M
        expected (test-common/to-sam-alignment
                  {:qname "Q", :flag (int 0),
                   :rname "ref", :pos (int 2), :end (int 3),
                   :mapq (int 0), :cigar "2M",
                   :rnext "*", :pnext (int 0), :tlen (int 0),
                   :seq "AA", :qual "*", :options []})]

    (testing "two-arguments-decode-alignment-test"
      (are [?expected-aln ?byte]
           (= ?expected-aln
              (decoder/decode-alignment
               [{:name "ref", :len 0}]
               {:data (byte-array (map unchecked-byte ?byte))}))
        expected
        no-opts

        expected
        with-CG))

    (testing "four-arguments-decode-alignment-test"
      (are [expected-aln byte start end]
           (= expected-aln
              (decoder/decode-alignment
               [{:name "ref", :len 0}]
               {:data (byte-array (map unchecked-byte byte))} start end))
        expected
        no-opts ;; pos 2, ref-end 3
        2 3

        nil
        no-opts
        1 1

        nil
        no-opts
        4 100

        expected
        with-CG ;; pos 2, ref-end 3
        2 2

        nil
        with-CG
        1 1

        nil
        with-CG
        4 (Math/pow 10 15)))))

(ns cljam.algo.convert-test
  (:require [clojure.test :refer [deftest is are testing]]
            [clojure.string :as cstr]
            [cljam.test-common :refer
             [with-before-after
              prepare-cache!
              clean-cache!
              not-throw?
              same-sam-contents?
              same-sequence-contents?
              temp-dir
              test-sam-file
              test-bam-file
              test-paired-bam-file
              test-fa-file
              test-twobit-file
              test-fq-file
              test-fq-r1-file
              test-fq-r2-file]]
            [cljam.algo.convert :as convert]
            [cljam.io.sequence :as cseq]
            [cljam.io.fastq :as fq]
            [clojure.java.io :as cio]))

(defn same-read-and-seq? [in out]
  (with-open [i (fq/reader in)
              o (cseq/reader out)]
    (->> (cseq/read-all-sequences o)
         (map (every-pred (juxt :name :sequence))
              (fq/read-sequences i {:decode-quality nil}))
         (every? true?))))

(defn read-test [x y]
  (let [f (juxt :sequence (comp first #(cstr/split % #"(/|\s)") :name))]
    (= (f x) (f y))))

(defn same-reads?
  ([original-r1 original-r2 output]
   (with-open [r1 (fq/reader original-r1)
               r2 (fq/reader original-r2)
               o (fq/reader output)]
     (->> (fq/read-sequences o {:decode-quality nil})
          (map read-test
               (interleave
                (fq/read-sequences r1 {:decode-quality nil})
                (fq/read-sequences r2 {:decode-quality nil})))
          (every? true?))))
  ([original output]
   (with-open [orig (fq/reader original)
               out (fq/reader output)]
     (->> (fq/read-sequences out {:decode-quality nil})
          (map read-test (fq/read-sequences orig {:decode-quality nil}))
          (every? true?)))))

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
    (testing "FASTQ -> FASTA"
      (let [in test-fq-file
            out (str temp-dir "/test_fq.fa")]
        (is (not-throw? (convert/convert in out)))
        (is (same-read-and-seq? in out))))
    (testing "FASTQ -> TwoBit"
      (let [in test-fq-file
            out (str temp-dir "/test_fq.2bit")]
        (is (not-throw? (convert/convert in out)))
        (is (same-read-and-seq? in out))))
    (testing "SAM -> FASTQ"
      (let [in test-paired-bam-file
            out (str temp-dir "/test_paired.fq")]
        (is (not-throw? (convert/convert in out)))
        (is (same-reads? test-fq-r1-file test-fq-r2-file out))))
    (testing "SAM -> FASTQ R1/R2"
      (let [in test-paired-bam-file
            out-r1 (str temp-dir "/test_R1.fq")
            out-r2 (str temp-dir "/test_R2.fq")]
        (is (not-throw? (convert/convert in [out-r1 out-r2])))
        (is (same-reads? test-fq-r1-file out-r1))
        (is (same-reads? test-fq-r2-file out-r2))))
    (testing "SAM -> FASTQ R0/R1/R2"
      (let [in test-paired-bam-file
            out-r0 (str temp-dir "/test_R0.fq")
            out-r1 (str temp-dir "/test_R1.fq")
            out-r2 (str temp-dir "/test_R2.fq")]
        (is (not-throw? (convert/convert in [out-r0 out-r1 out-r2])))
        (is (zero? (.length (cio/file out-r0))))
        (is (same-reads? test-fq-r1-file out-r1))
        (is (same-reads? test-fq-r2-file out-r2))))
    (testing "error"
      (are [in out] (thrown? Exception (convert/convert in [out]))
        test-bam-file (str temp-dir "/test.unknown")
        test-bam-file (str temp-dir "/test.2bit")))))

(deftest short-qname
  (are [?flag ?qname]
       (= (convert/short-qname {:flag ?flag, :qname "SEQ"}) {:flag ?flag, :qname ?qname})
    65  "SEQ/1"
    129 "SEQ/2"
    81  "SEQ/1"
    145 "SEQ/2"
    0   "SEQ"
    16  "SEQ"))

(deftest medium-qname
  (are [?flag ?qname]
       (= (convert/medium-qname {:flag ?flag, :qname "SEQ"}) {:flag ?flag, :qname ?qname})
    65  "SEQ_R1"
    129 "SEQ_R2"
    81  "SEQ_R1"
    145 "SEQ_R2"
    0   "SEQ"
    16  "SEQ"))

(deftest long-qname
  (are [?flag ?qname]
       (= (convert/long-qname {:flag ?flag, :qname "SEQ"}) {:flag ?flag, :qname ?qname})
    65  "SEQ 1:N:0:1"
    129 "SEQ 2:N:0:1"
    81  "SEQ 1:N:0:1"
    145 "SEQ 2:N:0:1"
    0   "SEQ"
    16  "SEQ"))

(deftest aln->read
  (are [?flag ?name ?seq ?qual]
       (= (into {} (convert/aln->read {:flag ?flag, :qname "SEQ", :seq "AATGC", :qual "IIIEE"}))
          {:name ?name, :sequence ?seq, :quality ?qual})
    65  "SEQ" "AATGC" "IIIEE"
    129 "SEQ" "AATGC" "IIIEE"
    81  "SEQ" "GCATT" "EEIII"
    145 "SEQ" "GCATT" "EEIII"
    0   "SEQ" "AATGC" "IIIEE"
    16  "SEQ" "GCATT" "EEIII")
  (is (= (into {} (convert/aln->read {:flag 16, :qname "SEQ", :seq "AATGC", :qual "*"}))
         {:name "SEQ", :sequence "GCATT", :quality "\"\"\"\"\""})))

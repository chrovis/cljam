(ns cljam.io.util-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.test-common :refer :all]
            [cljam.util :as util]
            [cljam.io.bed :as bed]
            [cljam.io.fastq :as fastq]
            [cljam.io.sam :as sam]
            [cljam.io.sequence :as cseq]
            [cljam.io.util :as io-util]
            [cljam.io.vcf :as vcf]
            [cljam.io.wig :as wig]
            [cljam.io.bigwig :as bigwig]))

(deftest about-file-type-detection
  (are [?path ?expected] (and (= (io-util/file-type ?path) ?expected)
                              (= (io-util/file-type (str "./" ?path)) ?expected)
                              (= (io-util/file-type (str "/home/bar/" ?path)) ?expected))
    "foo.bam" :bam
    "foo.BAM" :bam
    "foo.bam.bai" :bai
    "foo.bai" :bai
    "foo.sam" :sam
    "foo.SAM" :sam
    "foo.fa"  :fasta
    "foo.fasta" :fasta
    "foo.fa.gz" :fasta
    "foo.fa.bz2" :fasta
    "foo.fasta.gz" :fasta
    "foo.fasta.bz2" :fasta
    "foo.FA" :fasta
    "foo.FA.GZ" :fasta
    "foo.FASTA" :fasta
    "foo.FASTA.GZ" :fasta
    "foo.fas" :fasta
    "foo.fsa" :fasta
    "foo.seq" :fasta
    "foo.fna" :fasta
    "foo.faa" :fasta
    "foo.ffn" :fasta
    "foo.frn" :fasta
    "foo.mpfa" :fasta
    "foo.fai"  :fai
    "foo.fa.fai"  :fai
    "foo.2bit" :2bit
    "foo.fq" :fastq
    "foo.fq.gz" :fastq
    "foo.fastq" :fastq
    "foo.fastq.gz" :fastq
    "foo.vcf" :vcf
    "foo.vcf.gz" :vcf
    "foo.VCF" :vcf
    "foo.VCF.GZ" :vcf
    "foo.bcf" :bcf
    "foo.BCF" :bcf
    "foo.bed" :bed
    "foo.bed.gz" :bed
    "foo.BED" :bed
    "foo.BED.GZ" :bed
    "foo.wig" :wig
    "foo.WIG" :wig
    "foo.bigWig" :bigwig
    "foo.BIGWIG" :bigwig
    "foo.bw" :bigwig
    "foo.BW" :bigwig)
  (are [?dir]
      (are [?path] (thrown? Exception (io-util/file-type (str ?dir ?path)))
        "foo.bam.gz"
        "foo.SAM.gz"
        "foo.cram"
        "foo.CRAM"
        "foo.2bit.gz"
        "foo.bcf.gz")
    ""
    "./"
    "/home/bar"))

(deftest reader-predicates-test
  (testing "sam reader"
    (with-open [r (sam/reader test-sam-file)]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-reader? true
        io-util/sam-reader? true
        io-util/bam-reader? false
        io-util/variant-reader? false
        io-util/vcf-reader? false
        io-util/bcf-reader? false
        io-util/sequence-reader? false
        io-util/fasta-reader? false
        io-util/twobit-reader? false
        io-util/fastq-reader? false
        io-util/bed-reader? false
        io-util/wig-reader? false
        io-util/bigwig-reader? false)))
  (testing "bam reader"
    (with-open [r (sam/reader small-bam-file)]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-reader? true
        io-util/sam-reader? false
        io-util/bam-reader? true
        io-util/variant-reader? false
        io-util/vcf-reader? false
        io-util/bcf-reader? false
        io-util/sequence-reader? false
        io-util/fasta-reader? false
        io-util/twobit-reader? false
        io-util/fastq-reader? false
        io-util/bed-reader? false
        io-util/wig-reader? false
        io-util/bigwig-reader? false))
    (with-open [r (sam/reader small-bam-file)
                cloned (sam/reader r)]
      (is (true? (io-util/bam-reader? cloned))))
    (with-open [r (sam/reader test-sam-file)]
      (is (thrown? Exception (sam/reader r)))))
  (testing "vcf reader"
    (with-open [r (vcf/reader test-vcf-v4_3-file)]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-reader? false
        io-util/sam-reader? false
        io-util/bam-reader? false
        io-util/variant-reader? true
        io-util/vcf-reader? true
        io-util/bcf-reader? false
        io-util/sequence-reader? false
        io-util/fasta-reader? false
        io-util/twobit-reader? false
        io-util/fastq-reader? false
        io-util/bed-reader? false
        io-util/wig-reader? false
        io-util/bigwig-reader? false)))
  (testing "bcf reader"
    (with-open [r (vcf/reader test-bcf-v4_3-file)]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-reader? false
        io-util/sam-reader? false
        io-util/bam-reader? false
        io-util/variant-reader? true
        io-util/vcf-reader? false
        io-util/bcf-reader? true
        io-util/sequence-reader? false
        io-util/fasta-reader? false
        io-util/twobit-reader? false
        io-util/fastq-reader? false
        io-util/bed-reader? false
        io-util/wig-reader? false
        io-util/bigwig-reader? false)))
  (testing "fasta reader"
    (with-open [r (cseq/reader test-fa-file)]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-reader? false
        io-util/sam-reader? false
        io-util/bam-reader? false
        io-util/variant-reader? false
        io-util/vcf-reader? false
        io-util/bcf-reader? false
        io-util/sequence-reader? true
        io-util/fasta-reader? true
        io-util/twobit-reader? false
        io-util/fastq-reader? false
        io-util/bed-reader? false
        io-util/wig-reader? false
        io-util/bigwig-reader? false)))
  (testing "twobit reader"
    (with-open [r (cseq/reader test-twobit-file)]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-reader? false
        io-util/sam-reader? false
        io-util/bam-reader? false
        io-util/variant-reader? false
        io-util/vcf-reader? false
        io-util/bcf-reader? false
        io-util/sequence-reader? true
        io-util/fasta-reader? false
        io-util/twobit-reader? true
        io-util/fastq-reader? false
        io-util/bed-reader? false
        io-util/wig-reader? false
        io-util/bigwig-reader? false)))
  (testing "fastq reader"
    (with-open [r (fastq/reader test-fq-file)]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-reader? false
        io-util/sam-reader? false
        io-util/bam-reader? false
        io-util/variant-reader? false
        io-util/vcf-reader? false
        io-util/bcf-reader? false
        io-util/fasta-reader? false
        io-util/sequence-reader? false
        io-util/twobit-reader? false
        io-util/fastq-reader? true
        io-util/bed-reader? false
        io-util/wig-reader? false
        io-util/bigwig-reader? false)))
  (testing "bed reader"
    (with-open [r (bed/reader test-bed-file1)]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-reader? false
        io-util/sam-reader? false
        io-util/bam-reader? false
        io-util/variant-reader? false
        io-util/vcf-reader? false
        io-util/bcf-reader? false
        io-util/sequence-reader? false
        io-util/fasta-reader? false
        io-util/twobit-reader? false
        io-util/fastq-reader? false
        io-util/bed-reader? true
        io-util/wig-reader? false
        io-util/bigwig-reader? false)))
  (testing "wig reader"
    (with-open [r (wig/reader test-wig-file1)]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-reader? false
        io-util/sam-reader? false
        io-util/bam-reader? false
        io-util/variant-reader? false
        io-util/vcf-reader? false
        io-util/bcf-reader? false
        io-util/sequence-reader? false
        io-util/fasta-reader? false
        io-util/twobit-reader? false
        io-util/fastq-reader? false
        io-util/bed-reader? false
        io-util/wig-reader? true
        io-util/bigwig-reader? false)))
  (testing "bigwig reader"
    (with-open [r (bigwig/reader test-bigwig-fixed-file)]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-reader? false
        io-util/sam-reader? false
        io-util/bam-reader? false
        io-util/variant-reader? false
        io-util/vcf-reader? false
        io-util/bcf-reader? false
        io-util/sequence-reader? false
        io-util/fasta-reader? false
        io-util/twobit-reader? false
        io-util/fastq-reader? false
        io-util/bed-reader? false
        io-util/wig-reader? false
        io-util/bigwig-reader? true))))

(deftest writer-predicates-test
  (testing "sam writer"
    (with-open [r (sam/writer (.getAbsolutePath (cio/file util/temp-dir "temp.sam")))]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-writer? true
        io-util/sam-writer? true
        io-util/bam-writer? false
        io-util/sequence-writer? false
        io-util/fasta-writer? false
        io-util/twobit-writer? false
        io-util/variant-writer? false
        io-util/vcf-writer? false
        io-util/bcf-writer? false
        io-util/fastq-writer? false
        io-util/bed-writer? false
        io-util/wig-writer? false)))
  (testing "bam writer"
    (with-open [r (sam/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bam")))]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-writer? true
        io-util/sam-writer? false
        io-util/bam-writer? true
        io-util/sequence-writer? false
        io-util/fasta-writer? false
        io-util/twobit-writer? false
        io-util/variant-writer? false
        io-util/vcf-writer? false
        io-util/bcf-writer? false
        io-util/fastq-writer? false
        io-util/bed-writer? false
        io-util/wig-writer? false)))
  (testing "fasta writer"
    (with-open [r (cseq/writer (.getAbsolutePath (cio/file util/temp-dir "temp.fa")))]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-writer? false
        io-util/sam-writer? false
        io-util/bam-writer? false
        io-util/sequence-writer? true
        io-util/fasta-writer? true
        io-util/twobit-writer? false
        io-util/variant-writer? false
        io-util/vcf-writer? false
        io-util/bcf-writer? false
        io-util/fastq-writer? false
        io-util/bed-writer? false
        io-util/wig-writer? false)))
  (testing "twobit writer"
    (with-open [r (cseq/writer (.getAbsolutePath (cio/file util/temp-dir "temp.2bit")))]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-writer? false
        io-util/sam-writer? false
        io-util/bam-writer? false
        io-util/sequence-writer? true
        io-util/fasta-writer? false
        io-util/twobit-writer? true
        io-util/variant-writer? false
        io-util/vcf-writer? false
        io-util/bcf-writer? false
        io-util/fastq-writer? false
        io-util/bed-writer? false
        io-util/wig-writer? false)))
  (testing "vcf writer"
    (with-open [r (vcf/writer (.getAbsolutePath (cio/file util/temp-dir "temp.vcf")) {} [])]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-writer? false
        io-util/sam-writer? false
        io-util/bam-writer? false
        io-util/sequence-writer? false
        io-util/fasta-writer? false
        io-util/twobit-writer? false
        io-util/variant-writer?  true
        io-util/vcf-writer? true
        io-util/bcf-writer? false
        io-util/fastq-writer? false
        io-util/bed-writer? false
        io-util/wig-writer? false)))
  (testing "bcf writer"
    (with-open [r (vcf/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bcf")) {} [])]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-writer? false
        io-util/sam-writer? false
        io-util/bam-writer? false
        io-util/sequence-writer? false
        io-util/fasta-writer? false
        io-util/twobit-writer? false
        io-util/variant-writer?  true
        io-util/vcf-writer? false
        io-util/bcf-writer? true
        io-util/fastq-writer? false
        io-util/bed-writer? false
        io-util/wig-writer? false)))
  (testing "fastq writer"
    (with-open [r (fastq/writer (.getAbsolutePath (cio/file util/temp-dir "temp.fq")))]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-writer? false
        io-util/sam-writer? false
        io-util/bam-writer? false
        io-util/sequence-writer? false
        io-util/fasta-writer? false
        io-util/twobit-writer? false
        io-util/variant-writer?  false
        io-util/vcf-writer? false
        io-util/bcf-writer? false
        io-util/fastq-writer? true
        io-util/bed-writer? false
        io-util/wig-writer? false)))
  (testing "bed writer"
    (with-open [r (bed/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bed")))]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-writer? false
        io-util/sam-writer? false
        io-util/bam-writer? false
        io-util/sequence-writer? false
        io-util/fasta-writer? false
        io-util/twobit-writer? false
        io-util/variant-writer?  false
        io-util/vcf-writer? false
        io-util/bcf-writer? false
        io-util/fastq-writer? false
        io-util/bed-writer? true
        io-util/wig-writer? false)))
  (testing "wig writer"
    (with-open [r (wig/writer (.getAbsolutePath (cio/file util/temp-dir "temp.wig")))]
      (are [?pred ?expected] (= (?pred r) ?expected)
        io-util/alignment-writer? false
        io-util/sam-writer? false
        io-util/bam-writer? false
        io-util/sequence-writer? false
        io-util/fasta-writer? false
        io-util/twobit-writer? false
        io-util/variant-writer?  false
        io-util/vcf-writer? false
        io-util/bcf-writer? false
        io-util/fastq-writer? false
        io-util/bed-writer? false
        io-util/wig-writer? true))))

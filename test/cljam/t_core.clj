(ns cljam.t-core
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.core :as core]
            [cljam.core :as core]
            [cljam.util :as util]
            [cljam.util.sam-util :as sam-util]
            [cljam.t-common :as common]))

(deftest about-file-type-detection
  (are
      [?path ?expected]
      (and (= (core/file-type ?path) ?expected)
           (= (core/file-type (str "./" ?path)) ?expected)
           (= (core/file-type (str "/home/bar/" ?path)) ?expected))
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
    "foo.BED.GZ" :bed)

  (are [?dir]
      (are [?path]
          (thrown? Exception (core/file-type (str ?dir ?path)))
        "foo.bam.gz"
        "foo.SAM.gz"
        "foo.cram"
        "foo.CRAM"
        "foo.2bit.gz"
        "foo.bcf.gz")
    ""
    "./"
    "/home/bar"))

(deftest about-readers
  (with-open [r (core/reader common/test-sam-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-reader? true
      core/sam-reader? true
      core/bam-reader? false
      core/variant-reader? false
      core/vcf-reader? false
      core/bcf-reader? false
      core/sequence-reader? false
      core/fasta-reader? false
      core/twobit-reader? false
      core/fastq-reader? false
      core/bed-reader? false))

  (with-open [r (core/reader common/small-bam-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-reader? true
      core/sam-reader? false
      core/bam-reader? true
      core/variant-reader? false
      core/vcf-reader? false
      core/bcf-reader? false
      core/sequence-reader? false
      core/fasta-reader? false
      core/twobit-reader? false
      core/fastq-reader? false
      core/bed-reader? false))

  (with-open [r (core/reader common/test-vcf-v4_3-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-reader? false
      core/sam-reader? false
      core/bam-reader? false
      core/variant-reader? true
      core/vcf-reader? true
      core/bcf-reader? false
      core/sequence-reader? false
      core/fasta-reader? false
      core/twobit-reader? false
      core/fastq-reader? false
      core/bed-reader? false))

  (with-open [r (core/reader common/test-bcf-v4_3-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-reader? false
      core/sam-reader? false
      core/bam-reader? false
      core/variant-reader? true
      core/vcf-reader? false
      core/bcf-reader? true
      core/sequence-reader? false
      core/fasta-reader? false
      core/twobit-reader? false
      core/fastq-reader? false
      core/bed-reader? false))

  (with-open [r (core/reader common/test-fa-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-reader? false
      core/sam-reader? false
      core/bam-reader? false
      core/variant-reader? false
      core/vcf-reader? false
      core/bcf-reader? false
      core/sequence-reader? true
      core/fasta-reader? true
      core/twobit-reader? false
      core/fastq-reader? false
      core/bed-reader? false))

  (with-open [r (core/reader common/test-twobit-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-reader? false
      core/sam-reader? false
      core/bam-reader? false
      core/variant-reader? false
      core/vcf-reader? false
      core/bcf-reader? false
      core/sequence-reader? true
      core/fasta-reader? false
      core/twobit-reader? true
      core/fastq-reader? false
      core/bed-reader? false))

  (with-open [r (core/reader common/test-fq-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-reader? false
      core/sam-reader? false
      core/bam-reader? false
      core/variant-reader? false
      core/vcf-reader? false
      core/bcf-reader? false
      core/fasta-reader? false
      core/sequence-reader? false
      core/twobit-reader? false
      core/fastq-reader? true
      core/bed-reader? false))

  (with-open [r (core/reader common/test-bed-file1)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-reader? false
      core/sam-reader? false
      core/bam-reader? false
      core/variant-reader? false
      core/vcf-reader? false
      core/bcf-reader? false
      core/sequence-reader? false
      core/fasta-reader? false
      core/twobit-reader? false
      core/fastq-reader? false
      core/bed-reader? true))

  (with-open [r (core/reader common/small-bam-file)
              cloned (core/reader r)]
    (is (= (core/bam-reader? cloned) true)))

  (with-open [r (core/reader common/test-sam-file)]
    (is (thrown? Exception (core/reader r))))

  (is (thrown? Exception (core/reader "./test-resources/bam/foo.bam")))
  (is (thrown? Exception (core/reader common/test-bai-file)))
  (is (thrown? Exception (core/reader "./test-resources/bam/foo.baam"))))

(deftest about-writers
  (with-open [r (core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.sam")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-writer? true
      core/sam-writer? true
      core/bam-writer? false
      core/variant-writer? false
      core/vcf-writer? false
      core/bcf-writer? false
      core/fastq-writer? false
      core/bed-writer? false))

  (with-open [r (core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bam")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-writer? true
      core/sam-writer? false
      core/bam-writer? true
      core/variant-writer? false
      core/vcf-writer? false
      core/bcf-writer? false
      core/fastq-writer? false
      core/bed-writer? false))

  (with-open [r (core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.vcf")) {} [])]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-writer? false
      core/sam-writer? false
      core/bam-writer? false
      core/variant-writer?  true
      core/vcf-writer? true
      core/bcf-writer? false
      core/fastq-writer? false
      core/bed-writer? false))

  (with-open [r (core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bcf")) {} [])]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-writer? false
      core/sam-writer? false
      core/bam-writer? false
      core/variant-writer?  true
      core/vcf-writer? false
      core/bcf-writer? true
      core/fastq-writer? false
      core/bed-writer? false))

  (with-open [r (core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.fq")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-writer? false
      core/sam-writer? false
      core/bam-writer? false
      core/variant-writer?  false
      core/vcf-writer? false
      core/bcf-writer? false
      core/fastq-writer? true
      core/bed-writer? false))

  (with-open [r (core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bed")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      core/alignment-writer? false
      core/sam-writer? false
      core/bam-writer? false
      core/variant-writer?  false
      core/vcf-writer? false
      core/bcf-writer? false
      core/fastq-writer? false
      core/bed-writer? true))

  (is (thrown? Exception (core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.baam")))))
  (is (thrown? Exception (core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bai"))))))

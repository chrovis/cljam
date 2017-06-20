(ns cljam.io.t-core
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.util :as util]
            [cljam.io.core :as io-core]
            [cljam.t-common :as common]))

(deftest about-file-type-detection
  (are
      [?path ?expected]
      (and (= (io-core/file-type ?path) ?expected)
           (= (io-core/file-type (str "./" ?path)) ?expected)
           (= (io-core/file-type (str "/home/bar/" ?path)) ?expected))
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
          (thrown? Exception (io-core/file-type (str ?dir ?path)))
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
  (with-open [r (io-core/reader common/test-sam-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-reader? true
      io-core/sam-reader? true
      io-core/bam-reader? false
      io-core/variant-reader? false
      io-core/vcf-reader? false
      io-core/bcf-reader? false
      io-core/sequence-reader? false
      io-core/fasta-reader? false
      io-core/twobit-reader? false
      io-core/fastq-reader? false
      io-core/bed-reader? false))

  (with-open [r (io-core/reader common/small-bam-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-reader? true
      io-core/sam-reader? false
      io-core/bam-reader? true
      io-core/variant-reader? false
      io-core/vcf-reader? false
      io-core/bcf-reader? false
      io-core/sequence-reader? false
      io-core/fasta-reader? false
      io-core/twobit-reader? false
      io-core/fastq-reader? false
      io-core/bed-reader? false))

  (with-open [r (io-core/reader common/test-vcf-v4_3-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-reader? false
      io-core/sam-reader? false
      io-core/bam-reader? false
      io-core/variant-reader? true
      io-core/vcf-reader? true
      io-core/bcf-reader? false
      io-core/sequence-reader? false
      io-core/fasta-reader? false
      io-core/twobit-reader? false
      io-core/fastq-reader? false
      io-core/bed-reader? false))

  (with-open [r (io-core/reader common/test-bcf-v4_3-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-reader? false
      io-core/sam-reader? false
      io-core/bam-reader? false
      io-core/variant-reader? true
      io-core/vcf-reader? false
      io-core/bcf-reader? true
      io-core/sequence-reader? false
      io-core/fasta-reader? false
      io-core/twobit-reader? false
      io-core/fastq-reader? false
      io-core/bed-reader? false))

  (with-open [r (io-core/reader common/test-fa-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-reader? false
      io-core/sam-reader? false
      io-core/bam-reader? false
      io-core/variant-reader? false
      io-core/vcf-reader? false
      io-core/bcf-reader? false
      io-core/sequence-reader? true
      io-core/fasta-reader? true
      io-core/twobit-reader? false
      io-core/fastq-reader? false
      io-core/bed-reader? false))

  (with-open [r (io-core/reader common/test-twobit-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-reader? false
      io-core/sam-reader? false
      io-core/bam-reader? false
      io-core/variant-reader? false
      io-core/vcf-reader? false
      io-core/bcf-reader? false
      io-core/sequence-reader? true
      io-core/fasta-reader? false
      io-core/twobit-reader? true
      io-core/fastq-reader? false
      io-core/bed-reader? false))

  (with-open [r (io-core/reader common/test-fq-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-reader? false
      io-core/sam-reader? false
      io-core/bam-reader? false
      io-core/variant-reader? false
      io-core/vcf-reader? false
      io-core/bcf-reader? false
      io-core/fasta-reader? false
      io-core/sequence-reader? false
      io-core/twobit-reader? false
      io-core/fastq-reader? true
      io-core/bed-reader? false))

  (with-open [r (io-core/reader common/test-bed-file1)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-reader? false
      io-core/sam-reader? false
      io-core/bam-reader? false
      io-core/variant-reader? false
      io-core/vcf-reader? false
      io-core/bcf-reader? false
      io-core/sequence-reader? false
      io-core/fasta-reader? false
      io-core/twobit-reader? false
      io-core/fastq-reader? false
      io-core/bed-reader? true))

  (with-open [r (io-core/reader common/small-bam-file)
              cloned (io-core/reader r)]
    (is (= (io-core/bam-reader? cloned) true)))

  (with-open [r (io-core/reader common/test-sam-file)]
    (is (thrown? Exception (io-core/reader r))))

  (is (thrown? Exception (io-core/reader "./test-resources/bam/foo.bam")))
  (is (thrown? Exception (io-core/reader common/test-bai-file)))
  (is (thrown? Exception (io-core/reader "./test-resources/bam/foo.baam"))))

(deftest about-writers
  (with-open [r (io-core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.sam")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-writer? true
      io-core/sam-writer? true
      io-core/bam-writer? false
      io-core/sequence-writer? false
      io-core/fasta-writer? false
      io-core/twobit-writer? false
      io-core/variant-writer? false
      io-core/vcf-writer? false
      io-core/bcf-writer? false
      io-core/fastq-writer? false
      io-core/bed-writer? false))

  (with-open [r (io-core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bam")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-writer? true
      io-core/sam-writer? false
      io-core/bam-writer? true
      io-core/sequence-writer? false
      io-core/fasta-writer? false
      io-core/twobit-writer? false
      io-core/variant-writer? false
      io-core/vcf-writer? false
      io-core/bcf-writer? false
      io-core/fastq-writer? false
      io-core/bed-writer? false))

  (with-open [r (io-core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.fa")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-writer? false
      io-core/sam-writer? false
      io-core/bam-writer? false
      io-core/sequence-writer? true
      io-core/fasta-writer? true
      io-core/twobit-writer? false
      io-core/variant-writer? false
      io-core/vcf-writer? false
      io-core/bcf-writer? false
      io-core/fastq-writer? false
      io-core/bed-writer? false))

  (with-open [r (io-core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.2bit")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-writer? false
      io-core/sam-writer? false
      io-core/bam-writer? false
      io-core/sequence-writer? true
      io-core/fasta-writer? false
      io-core/twobit-writer? true
      io-core/variant-writer? false
      io-core/vcf-writer? false
      io-core/bcf-writer? false
      io-core/fastq-writer? false
      io-core/bed-writer? false))

  (with-open [r (io-core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.vcf")) {} [])]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-writer? false
      io-core/sam-writer? false
      io-core/bam-writer? false
      io-core/sequence-writer? false
      io-core/fasta-writer? false
      io-core/twobit-writer? false
      io-core/variant-writer?  true
      io-core/vcf-writer? true
      io-core/bcf-writer? false
      io-core/fastq-writer? false
      io-core/bed-writer? false))

  (with-open [r (io-core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bcf")) {} [])]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-writer? false
      io-core/sam-writer? false
      io-core/bam-writer? false
      io-core/sequence-writer? false
      io-core/fasta-writer? false
      io-core/twobit-writer? false
      io-core/variant-writer?  true
      io-core/vcf-writer? false
      io-core/bcf-writer? true
      io-core/fastq-writer? false
      io-core/bed-writer? false))

  (with-open [r (io-core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.fq")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-writer? false
      io-core/sam-writer? false
      io-core/bam-writer? false
      io-core/sequence-writer? false
      io-core/fasta-writer? false
      io-core/twobit-writer? false
      io-core/variant-writer?  false
      io-core/vcf-writer? false
      io-core/bcf-writer? false
      io-core/fastq-writer? true
      io-core/bed-writer? false))

  (with-open [r (io-core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bed")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
      io-core/alignment-writer? false
      io-core/sam-writer? false
      io-core/bam-writer? false
      io-core/sequence-writer? false
      io-core/fasta-writer? false
      io-core/twobit-writer? false
      io-core/variant-writer?  false
      io-core/vcf-writer? false
      io-core/bcf-writer? false
      io-core/fastq-writer? false
      io-core/bed-writer? true))

  (is (thrown? Exception (io-core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.baam")))))
  (is (thrown? Exception (io-core/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bai"))))))

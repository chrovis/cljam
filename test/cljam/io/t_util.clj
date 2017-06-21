(ns cljam.io.t-util
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.util :as util]
            [cljam.io.util :as io-util]
            [cljam.t-common :as common]))

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

;; FIXME
#_(deftest about-readers
  (with-open [r (io-util/reader common/test-sam-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-reader? false))

  (with-open [r (io-util/reader common/small-bam-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-reader? false))

  (with-open [r (io-util/reader common/test-vcf-v4_3-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-reader? false))

  (with-open [r (io-util/reader common/test-bcf-v4_3-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-reader? false))

  (with-open [r (io-util/reader common/test-fa-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-reader? false))

  (with-open [r (io-util/reader common/test-twobit-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-reader? false))

  (with-open [r (io-util/reader common/test-fq-file)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-reader? false))

  (with-open [r (io-util/reader common/test-bed-file1)]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-reader? true))

  (with-open [r (io-util/reader common/small-bam-file)
              cloned (io-util/reader r)]
    (is (= (io-util/bam-reader? cloned) true)))

  (with-open [r (io-util/reader common/test-sam-file)]
    (is (thrown? Exception (io-util/reader r))))

  (is (thrown? Exception (io-util/reader "./test-resources/bam/foo.bam")))
  (is (thrown? Exception (io-util/reader common/test-bai-file)))
  (is (thrown? Exception (io-util/reader "./test-resources/bam/foo.baam"))))

#_(deftest about-writers
  (with-open [r (io-util/writer (.getAbsolutePath (cio/file util/temp-dir "temp.sam")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-writer? false))

  (with-open [r (io-util/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bam")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-writer? false))

  (with-open [r (io-util/writer (.getAbsolutePath (cio/file util/temp-dir "temp.fa")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-writer? false))

  (with-open [r (io-util/writer (.getAbsolutePath (cio/file util/temp-dir "temp.2bit")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-writer? false))

  (with-open [r (io-util/writer (.getAbsolutePath (cio/file util/temp-dir "temp.vcf")) {} [])]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-writer? false))

  (with-open [r (io-util/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bcf")) {} [])]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-writer? false))

  (with-open [r (io-util/writer (.getAbsolutePath (cio/file util/temp-dir "temp.fq")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-writer? false))

  (with-open [r (io-util/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bed")))]
    (are [?pred ?expected]
        (= (?pred r) ?expected)
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
      io-util/bed-writer? true))

  (is (thrown? Exception (io-util/writer (.getAbsolutePath (cio/file util/temp-dir "temp.baam")))))
  (is (thrown? Exception (io-util/writer (.getAbsolutePath (cio/file util/temp-dir "temp.bai"))))))

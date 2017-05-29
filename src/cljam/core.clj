(ns cljam.core
  "Core features of cljam."
  (:require [cljam.io :as io]
            [cljam.sam :as sam]
            [cljam.bam :as bam]
            [cljam.fasta :as fasta]
            [cljam.twobit :as twobit]
            [cljam.fastq :as fastq]
            [cljam.vcf :as vcf]
            [cljam.bcf :as bcf]
            [cljam.bed :as bed]
            [cljam.pileup.mpileup :as mplp])
  (:import [java.io Closeable]))

(defn alignment-reader?
  "Checks if given object implements protocol IAlignmentReader."
  [rdr]
  (satisfies? cljam.io/IAlignmentReader rdr))

(defn alignment-writer?
  "Checks if given object implements protocol IAlignmentWriter."
  [wtr]
  (satisfies? cljam.io/IAlignmentWriter wtr))

(defn sam-reader?
  "Checks if given object is an instance of SAMReader."
  [rdr]
  (instance? cljam.sam.reader.SAMReader rdr))

(defn sam-writer?
  "Checks if given object is an instance of SAMWriter."
  [wtr]
  (instance? cljam.sam.writer.SAMWriter wtr))

(defn bam-reader?
  "Checks if given object is an instance of BAMReader."
  [rdr]
  (instance? cljam.bam.reader.BAMReader rdr))

(defn bam-writer?
  "Checks if given object is an instance of BAMWriter."
  [wtr]
  (instance? cljam.bam.writer.BAMWriter wtr))

(defn variant-reader?
  "Checks if given object implements protocol IVariantReader."
  [rdr]
  (satisfies? cljam.io/IVariantReader rdr))

(defn variant-writer?
  "Checks if given object implements protocol IVariantWriter."
  [wtr]
  (satisfies? cljam.io/IVariantWriter wtr))

(defn vcf-reader?
  "Checks if given object is an instance of VCFReader."
  [rdr]
  (instance? cljam.vcf.reader.VCFReader rdr))

(defn vcf-writer?
  "Checks if given object is an instance of VCFWriter."
  [wtr]
  (instance? cljam.vcf.writer.VCFWriter wtr))

(defn bcf-reader?
  "Checks if given object is an instance of BCFReader."
  [rdr]
  (instance? cljam.bcf.reader.BCFReader rdr))

(defn bcf-writer?
  "Checks if given object is an instance of BCFWriter."
  [wtr]
  (instance? cljam.bcf.writer.BCFWriter wtr))

(defn sequence-reader?
  "Checks if given object implements protocol ISequenceReader."
  [rdr]
  (satisfies? cljam.io/ISequenceReader rdr))

(defn fasta-reader?
  "Checks if given object is an instance of FASTAReader."
  [rdr]
  (instance? cljam.fasta.reader.FASTAReader rdr))

(defn twobit-reader?
  "Checks if given object is an instance of TwoBitReader."
  [rdr]
  (instance? cljam.twobit.TwoBitReader rdr))

(defn fastq-reader?
  "Checks if given object is an instance of FASTQReader."
  [rdr]
  (instance? cljam.fastq.FASTQReader rdr))

(defn fastq-writer?
  "Checks if given object is an instance of FASTQWriter"
  [wtr]
  (instance? cljam.fastq.FASTQWriter wtr))

(defn bed-reader?
  "Checks if given object is an instance of BEDReader"
  [rdr]
  (instance? cljam.bed.BEDReader rdr))

(defn bed-writer?
  "Checks if given object is an instance of BEDWriter"
  [wtr]
  (instance? cljam.bed.BEDWriter wtr))

(defn file-type
  "Detects file format from input path string."
  [f]
  (condp re-find f
    #"(?i)\.sam$" :sam
    #"(?i)\.bai$" :bai
    #"(?i)\.bam$" :bam
    #"(?i)\.f(ast)?q" :fastq
    #"(?i)\.fai$" :fai
    #"(?i)\.fa(sta)?" :fasta
    #"(?i)\.2bit$" :2bit
    #"(?i)\.vcf" :vcf
    #"(?i)\.bcf$" :bcf
    #"(?i)\.bed" :bed
    (throw (IllegalArgumentException. "Invalid file type"))))

(defn ^Closeable reader
  "Selects suitable reader from f's extension, returning the reader.
  Opens a new reader if given a path, clones the reader if given a reader.
  This function supports SAM,BAM,FASTA,2BIT,FASTQ,VCF,BCF and BED format."
  [f & {:keys [ignore-index] :or {ignore-index false}}]
  (if (string? f)
    (case (file-type f)
      :sam (sam/reader f)
      :bam (bam/reader f :ignore-index ignore-index)
      :fasta (fasta/reader f :ignore-index ignore-index)
      :2bit (twobit/reader f)
      :fastq (fastq/reader f)
      :vcf (vcf/reader f)
      :bcf (bcf/reader f)
      :bed (bed/reader f)
      :else (throw (IllegalArgumentException. "Invalid file type")))
    (cond
      (bam-reader? f) (bam/clone-reader f)
      :else (throw (IllegalArgumentException. "Invalid reader type")))))

(defn ^Closeable writer
  "Selects suitable writer from f's extension, returning the writer.
  This function supports SAM,BAM,FASTQ,VCF,BCF and BED format."
  [f & args]
  (case (file-type f)
    :sam (sam/writer f)
    :bam (bam/writer f)
    :fastq (fastq/writer f)
    :vcf (apply vcf/writer f args)
    :bcf (apply bcf/writer f args)
    :bed (bed/writer f)
    :else (throw (IllegalArgumentException. "Invalid file type"))))

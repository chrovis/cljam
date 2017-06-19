(ns cljam.io.core
  "Core I/O features of cljam."
  (:require [cljam.io :as io]
            [cljam.io.sam :as sam]
            [cljam.io.bam :as bam]
            [cljam.io.fasta :as fasta]
            [cljam.io.twobit :as twobit]
            [cljam.io.fastq :as fastq]
            [cljam.io.vcf :as vcf]
            [cljam.io.bcf :as bcf]
            [cljam.io.bed :as bed])
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
  (instance? cljam.io.sam.reader.SAMReader rdr))

(defn sam-writer?
  "Checks if given object is an instance of SAMWriter."
  [wtr]
  (instance? cljam.io.sam.writer.SAMWriter wtr))

(defn bam-reader?
  "Checks if given object is an instance of BAMReader."
  [rdr]
  (instance? cljam.io.bam.reader.BAMReader rdr))

(defn bam-writer?
  "Checks if given object is an instance of BAMWriter."
  [wtr]
  (instance? cljam.io.bam.writer.BAMWriter wtr))

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
  (instance? cljam.io.vcf.reader.VCFReader rdr))

(defn vcf-writer?
  "Checks if given object is an instance of VCFWriter."
  [wtr]
  (instance? cljam.io.vcf.writer.VCFWriter wtr))

(defn bcf-reader?
  "Checks if given object is an instance of BCFReader."
  [rdr]
  (instance? cljam.io.bcf.reader.BCFReader rdr))

(defn bcf-writer?
  "Checks if given object is an instance of BCFWriter."
  [wtr]
  (instance? cljam.io.bcf.writer.BCFWriter wtr))

(defn sequence-reader?
  "Checks if given object implements protocol ISequenceReader."
  [rdr]
  (satisfies? cljam.io/ISequenceReader rdr))

(defn sequence-writer?
  "Checks if given object implements protocol ISequenceWriter."
  [wtr]
  (satisfies? cljam.io/ISequenceWriter wtr))

(defn fasta-reader?
  "Checks if given object is an instance of FASTAReader."
  [rdr]
  (instance? cljam.io.fasta.reader.FASTAReader rdr))

(defn fasta-writer?
  "Checks if given object is an instance of FASTAWriter."
  [wtr]
  (instance? cljam.io.fasta.writer.FASTAWriter wtr))

(defn twobit-reader?
  "Checks if given object is an instance of TwoBitReader."
  [rdr]
  (instance? cljam.io.twobit.reader.TwoBitReader rdr))

(defn twobit-writer?
  "Checks if given object is an instance of TwoBitWriter."
  [wtr]
  (instance? cljam.io.twobit.writer.TwoBitWriter wtr))

(defn fastq-reader?
  "Checks if given object is an instance of FASTQReader."
  [rdr]
  (instance? cljam.io.fastq.FASTQReader rdr))

(defn fastq-writer?
  "Checks if given object is an instance of FASTQWriter"
  [wtr]
  (instance? cljam.io.fastq.FASTQWriter wtr))

(defn bed-reader?
  "Checks if given object is an instance of BEDReader"
  [rdr]
  (instance? cljam.io.bed.BEDReader rdr))

(defn bed-writer?
  "Checks if given object is an instance of BEDWriter"
  [wtr]
  (instance? cljam.io.bed.BEDWriter wtr))

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
  This function supports SAM,BAM,FASTA,2BIT,FASTQ,VCF,BCF and BED format."
  [f & args]
  (case (file-type f)
    :sam (sam/writer f)
    :bam (bam/writer f)
    :fasta (apply fasta/writer f args)
    :2bit (twobit/writer f)
    :fastq (fastq/writer f)
    :vcf (apply vcf/writer f args)
    :bcf (apply bcf/writer f args)
    :bed (bed/writer f)
    :else (throw (IllegalArgumentException. "Invalid file type"))))

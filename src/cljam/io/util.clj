(ns cljam.io.util
  "I/O utilities."
  (:require [cljam.io.protocols :as protocols]
            cljam.io.sam.reader
            cljam.io.sam.writer
            cljam.io.bam.reader
            cljam.io.bam.writer
            cljam.io.vcf.reader
            cljam.io.vcf.writer
            cljam.io.bcf.reader
            cljam.io.bcf.writer
            cljam.io.fasta.reader
            cljam.io.fasta.writer
            cljam.io.twobit.reader
            cljam.io.twobit.writer
            cljam.io.fastq
            cljam.io.bed
            [cljam.util :as util]))

(defn alignment-reader?
  "Checks if given object implements protocol IAlignmentReader."
  [rdr]
  (satisfies? protocols/IAlignmentReader rdr))

(defn alignment-writer?
  "Checks if given object implements protocol IAlignmentWriter."
  [wtr]
  (satisfies? protocols/IAlignmentWriter wtr))

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
  (satisfies? protocols/IVariantReader rdr))

(defn variant-writer?
  "Checks if given object implements protocol IVariantWriter."
  [wtr]
  (satisfies? protocols/IVariantWriter wtr))

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
  (satisfies? protocols/ISequenceReader rdr))

(defn sequence-writer?
  "Checks if given object implements protocol ISequenceWriter."
  [wtr]
  (satisfies? protocols/ISequenceWriter wtr))

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
  "Checks if given object is an instance of BEDReader."
  [rdr]
  (instance? cljam.io.bed.BEDReader rdr))

(defn bed-writer?
  "Checks if given object is an instance of BEDWriter."
  [wtr]
  (instance? cljam.io.bed.BEDWriter wtr))

(defn file-type
  "Detects a file format from path string f, returning a keyword representing
  the format. Throws an exception if an unsupported file is supplied."
  [f]
  (condp re-find (.getPath (util/as-url f))
    #"(?i)\.sam$" :sam
    #"(?i)\.bai$" :bai
    #"(?i)\.bam$" :bam
    #"(?i)\.f(ast)?q" :fastq
    #"(?i)\.fai$" :fai
    #"(?i)\.(fa|fasta|fas|fsa|seq|fna|faa|ffn|frn|mpfa)" :fasta
    #"(?i)\.2bit$" :2bit
    #"(?i)\.vcf" :vcf
    #"(?i)\.bcf$" :bcf
    #"(?i)\.bed" :bed
    (throw (IllegalArgumentException. "Invalid file type"))))

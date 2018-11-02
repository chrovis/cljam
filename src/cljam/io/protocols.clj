(ns cljam.io.protocols
  "Protocols of reader/writer for various file formats."
  (:refer-clojure :exclude [read indexed?]))

(defrecord SAMAlignment
  [qname ^int flag rname ^int pos ^int end ^int mapq cigar rnext ^int pnext ^int tlen seq qual options])
(defrecord SAMRegionBlock [data ^int ref-id ^int pos ^int end])
(defrecord SAMCoordinateBlock [data ^int ref-id ^int pos ^int flag])
(defrecord SAMQuerynameBlock [data qname ^int flag])

(defprotocol IReader
  (reader-url [this]
    "Returns the file's URL.")
  (read [this] [this option]
    "Sequentially reads contents of the file.")
  (indexed? [this]
    "Returns true if the reader can be randomly accessed, false if not."))

(defprotocol IWriter
  (writer-url [this]
    "Returns the source's URL."))

(defprotocol IRegionReader
  (read-in-region [this region] [this region option]
    "Reads contents of the file in given region."))

(defprotocol IAlignmentReader
  (read-header [this]
    "Returns header of the SAM/BAM file.")
  (read-refs [this]
    "Returns references of the SAM/BAM file.")
  (read-alignments [this] [this region]
    "Reads alignments of the SAM/BAM file, returning the alignments as an eduction.")
  (read-blocks [this] [this region] [this region option]
    "Reads alignment blocks of the SAM/BAM file, returning the blocks as an eduction."))

(defprotocol IAlignmentWriter
  (write-header [this header]
    "Writes header to the SAM/BAM file.")
  (write-refs [this header]
    "Writes references to the SAM/BAM file.")
  (write-alignments [this alignments header]
    "Writes alignments to the SAM/BAM file.")
  (write-blocks [this blocks]
    "Writes alignment blocks of the SAM/BAM file."))

(defprotocol IVariantReader
  (meta-info [this]
    "Returns meta-info section of VCF/BCF file as a map.")
  (header [this]
    "Returns header of VCF/BCF file as a sequence of strings.")
  (read-variants [this] [this option]
    "Reads variants of the VCF/BCF file, returning them as a lazy sequence."))

(defprotocol IVariantWriter
  (write-variants [this variants]
    "Writes variants to thee VCF/BCF file."))

(defprotocol ISequenceReader
  (read-indices [this]
    "Reads metadata of indexed sequences in FASTA/2BIT file.")
  (read-all-sequences [this] [this option]
    "Reads all sequences of FASTA/2BIT file.")
  (read-sequence [this region] [this region option]
    "Reads sequence in region of FASTA/2BIT file."))

(defprotocol ISequenceWriter
  (write-sequences [this seqs]
    "Writes all sequences to FASTA/2BIT file."))

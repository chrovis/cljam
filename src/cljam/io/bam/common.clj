(ns cljam.io.bam.common)

(def bam-magic
  "The magic number of the BAM file format."
  "BAM\1")

(def ^:const fixed-block-size
  "The number of bytes for the fields with a fixed size of a BAM alignment."
  32)

(ns cljam.io
  "Protocols of SAM/BAM reader.")

(defrecord SAMAlignment
  [qname ^int flag rname ^int pos ^int end ^int mapq cigar rnext ^int pnext ^int tlen seq qual options])

(defprotocol ISAMReader
  (reader-path [this] "Returns the file's absolute path.")
  (read-header [this] "Returns header of the SAM/BAM file.")
  (read-refs [this] "Returns references of the SAM/BAM file.")
  (read-alignments [this] [this option]
    "Reads alignments of the SAM/BAM file, returning the alignments as a lazy
    sequence.")
  (read-blocks [this] [this option]
    "Reads alignment blocks of the SAM/BAM file, returning the blocks as a lazy
    sequence."))

(defprotocol ISAMWriter
  (writer-path [this] "Returns the file's absolute path.")
  (write-header [this header] "Writes header to the SAM/BAM file.")
  (write-refs [this header] "Writes references to the SAM/BAM file.")
  (write-alignments [this alignments header]
    "Writes alignments to the SAM/BAM file.")
  (write-blocks [this blocks]
    "Writes alignment blocks of the SAM/BAM file."))

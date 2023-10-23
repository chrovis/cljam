(ns cljam.io.bam-index.common)

(def bai-magic
  "The magic number of BAI file."
  "BAI\1")

(def ^:const max-bins
  "Maximum bin number or magic bin number."
  37450)

(def level-starts
  "The first bin numbers of each levels."
  [0 1 9 73 585 4681])

(def ^:const linear-index-shift
  "Minimum shift size for searching binning."
  14)

(def ^:const linear-index-depth
  "The depth of tree for searching binning."
  5)

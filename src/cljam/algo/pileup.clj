(ns cljam.algo.pileup
  "Functions to calculate pileup from the BAM."
  (:require [cljam.io.sam :as sam]
            [cljam.algo.pileup.mpileup :as mplp]))

(defn mpileup
  "Returns a lazy sequence of cljam.algo.pileup.mpileup.MPileupElement
  calculated from FASTA and BAM. If start and end are not supplied, piles whole
  range up."
  ([bam-reader region] (mpileup nil bam-reader region))
  ([ref-reader bam-reader region] (mplp/pileup ref-reader bam-reader region)))

(defn create-mpileup
  "Creates a mpileup file from the BAM file."
  [in-bam out-mplp]
  (with-open [r (sam/bam-reader in-bam)]
    (mplp/create-mpileup out-mplp nil r)))

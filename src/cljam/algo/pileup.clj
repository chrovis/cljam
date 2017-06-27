(ns cljam.algo.pileup
  "Functions to calculate pileup from the BAM."
  (:require [cljam.common :refer [*n-threads*]]
            [cljam.io.sam :as sam]
            [cljam.algo.pileup.pileup :as plp]
            [cljam.algo.pileup.mpileup :as mplp]))

(defn first-pos
  "Returns a position of first alignment in left-right, or nil."
  [bam-reader region]
  (plp/first-pos bam-reader region))

(def ^:private default-pileup-option
  {:n-threads 0})

(defn pileup
  "Piles alignments up, returning the pileup as a lazy seq. Requires a
  cljam.bam.reader.BAMReader instance and region. If start and end are not
  supplied, piles whole range up."
  ([reader region]
   (pileup reader region {}))
  ([bam-reader {:keys [chr start end] :or {start -1 end -1} :as region} option]
   (let [option* (merge default-pileup-option option)]
     (binding [*n-threads* (:n-threads option*)]
       (apply plp/pileup bam-reader region (apply concat option*))))))

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

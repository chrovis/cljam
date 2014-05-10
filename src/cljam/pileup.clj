(ns cljam.pileup
  (:require [clojure.string :as str]
            [clojure.java.io :refer [writer]]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.pileup.common :as plpc]
            [cljam.pileup.pileup :as plp]
            [cljam.pileup.mpileup :as mplp]))

(def window-width plpc/window-width)

(def first-pos plp/first-pos)

(def pileup plp/pileup)

(def mpileup mplp/pileup)

(defn create-mpileup
  "Creates a mpileup file from the BAM file."
  [in-bam out-mplp]
  (with-open [r (bam/reader in-bam)]
    (mplp/create-mpileup out-mplp r)))

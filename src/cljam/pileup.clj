(ns cljam.pileup
  (:require [clojure.java.io :refer [writer]]
            [cljam.common :refer [*n-threads*]]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.pileup.common :as plpc]
            [cljam.pileup.pileup :as plp]
            [cljam.pileup.mpileup :as mplp]))

(def window-width plpc/window-width)

(def first-pos plp/first-pos)

(def ^:private default-pileup-option
  {:n-threads 0})

(defn pileup
  ([bam-reader rname option]
   (pileup bam-reader rname -1 -1 option))
  ([bam-reader rname start end option]
   (let [option* (merge default-pileup-option option)]
     (binding [*n-threads* (:n-threads option*)]
       (apply plp/pileup bam-reader rname start end (apply concat option*))))))

(def mpileup mplp/pileup)

(defn create-mpileup
  "Creates a mpileup file from the BAM file."
  [in-bam out-mplp]
  (with-open [r (bam/reader in-bam)]
    (mplp/create-mpileup out-mplp nil r)))

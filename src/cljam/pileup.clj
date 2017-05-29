(ns cljam.pileup
  (:require [cljam.common :refer [*n-threads*]]
            [cljam.bam :as bam]
            [cljam.pileup.common :as plpc]
            [cljam.pileup.pileup :as plp]
            [cljam.pileup.mpileup :as mplp]))

(def window-width plpc/window-width)

(def first-pos plp/first-pos)

(def ^:private default-pileup-option
  {:n-threads 0})

(defn pileup
  ([reader region]
   (pileup reader region {}))
  ([bam-reader {:keys [chr start end] :or {start -1 end -1} :as region} option]
   (let [option* (merge default-pileup-option option)]
     (binding [*n-threads* (:n-threads option*)]
       (apply plp/pileup bam-reader region (apply concat option*))))))

(def mpileup mplp/pileup)

(defn create-mpileup
  "Creates a mpileup file from the BAM file."
  [in-bam out-mplp]
  (with-open [r (bam/reader in-bam)]
    (mplp/create-mpileup out-mplp nil r)))

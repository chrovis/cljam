(ns cljam.algo.pileup
  "Functions to calculate pileup from the BAM."
  (:require [cljam.io.sam :as sam]
            [cljam.algo.pileup.mpileup :as mplp]
            [clojure.java.io :as cio]
            [cljam.io.sequence :as cseq])
  (:import [java.io Closeable]))

(defn mpileup
  "Returns a lazy sequence of cljam.algo.pileup.mpileup.MPileupElement
  calculated from FASTA and BAM. If start and end are not supplied, piles whole
  range up. "
  ([sam-reader region] (mplp/pileup sam-reader region)))

(defn create-mpileup
  "Creates a mpileup file from the BAM file."
  ([in-sam out-mplp]
   (create-mpileup in-sam nil out-mplp))
  ([in-sam in-ref out-mplp]
   (create-mpileup in-sam in-ref out-mplp nil))
  ([in-sam in-ref out-mplp region]
   (with-open [s (sam/reader in-sam)
               w (cio/writer out-mplp)]
     (let [r (some-> in-ref cseq/reader)
           regs (if region
                  [region]
                  (map
                   (fn [{:keys [name len]}]
                     {:chr name :start 1 :end len})
                   (sam/read-refs s)))]
       (try
         (doseq [reg regs]
           (mplp/create-mpileup s r w reg))
         (finally
           (when r
             (.close ^Closeable r))))))))

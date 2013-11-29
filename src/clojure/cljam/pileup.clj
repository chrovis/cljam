(ns cljam.pileup
  (:require [clojure.string :as str]
            [clojure.java.io :refer [writer]]
            [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.pileup.pileup :as plp]
            [cljam.pileup.mpileup :as mplp]))

(def pileup plp/pileup)

(def mpileup mplp/pileup)

(defn create-mpileup
  [in-bam out-mplp]
  (with-open [r (bam/reader in-bam)
              w (writer out-mplp)]
    (doseq [rname (map :name (io/read-refs r))
            line  (mpileup r rname)]
      (when-not (zero? (:count line))
        (.write w (str/join \tab (map val line)))
        (.newLine w))))
  nil)

(ns cljam.normal
  (:require [clojure.java.io :refer [file]]
            (cljam [sam :as sam]
                   [bam :as bam]
                   [common :refer [version]]
                   [util :as util]
                   [io :as io])
            [cljam.util :refer [swap]]
            [cljam.util.chromosome :refer [normalize-chromosome-key]]))

;; TODO add test cases
;; TODO add functions to core.clj

(def ^:private chunk-size 1500000)

(defn- normalize-header
  [hdr]
  (let [seq (:SQ hdr)]
    (assoc hdr
      :SQ (vec (map #(swap % :SN normalize-chromosome-key) seq)))))

(defmulti normalize (fn [rdr wtr]
                      (case (str (type rdr))
                        "class cljam.sam.reader.SAMReader" :sam
                        "class cljam.bam.reader.BAMReader" :bam
                        nil)))

(defmethod normalize :sam
  [rdr wtr]
  ;; TODO implement
  )

(defmethod normalize :bam
  [rdr wtr]
  (let [hdr (normalize-header (io/read-header rdr))]
    (io/write-header wtr hdr)
    (io/write-refs wtr hdr)
    ;; TODO copy all rest of stream for performance. (do not read, parse and write)
    (doseq [blks (partition-all chunk-size (io/read-blocks rdr))]
      (io/write-blocks wtr blks))))

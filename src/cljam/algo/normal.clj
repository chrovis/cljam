(ns cljam.algo.normal
  (:require [clojure.java.io :refer [file]]
            [cljam.io.sam :as sam]
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
                        "class cljam.io.sam.reader.SAMReader" :sam
                        "class cljam.io.bam.reader.BAMReader" :bam
                        nil)))

(defmethod normalize :sam
  [rdr wtr]
  ;; TODO implement
  )

(defmethod normalize :bam
  [rdr wtr]
  (let [hdr (normalize-header (sam/read-header rdr))]
    (sam/write-header wtr hdr)
    (sam/write-refs wtr hdr)
    ;; TODO copy all rest of stream for performance. (do not read, parse and write)
    (doseq [blks (partition-all chunk-size (sam/read-blocks rdr))]
      (sam/write-blocks wtr blks))))

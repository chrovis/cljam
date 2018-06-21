(ns cljam.io.fasta-index.reader
  "Reading FASTA index file"
  (:require [proton.core :refer [as-long]]))

;;;; FAIReader

(deftype FAIReader [indices url])

;;;; Reader

(defn parse-fai
  [rdr]
  (apply hash-map
         (flatten (map
                   (fn [l]
                     (let [m (re-matches #"(.+?)\t(\d+?)\t(\d+)\t(\d+)\t(\d+)" l)]
                       [(nth m 1 nil) {:len (as-long (nth m 2 nil))
                                       :offset (as-long (nth m 3 nil))
                                       :line-blen (as-long (nth m 4 nil))
                                       :line-len (as-long (nth m 5 nil))}]))
                   (line-seq rdr)))))

(ns cljam.fasta-index.reader
  "Reading FASTA index file"
  (:require [cljam.util :refer [str->int]]))

;;;; FAIReader

(deftype FAIReader [indices f])

;;;; Reader

(defn parse-fai
  [rdr]
  (apply hash-map
         (flatten (map
                   (fn [l]
                     (let [m (re-matches #"(.+?)\t(\d+?)\t(\d+)\t(\d+)\t(\d+)" l)]
                       [(nth m 1 nil) {:len (str->int (nth m 2 nil))
                                       :offset (str->int (nth m 3 nil))
                                       :line-blen (str->int (nth m 4 nil))
                                       :line-len (str->int (nth m 5 nil))}]))
                   (line-seq rdr)))))

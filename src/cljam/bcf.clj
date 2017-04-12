(ns cljam.bcf
  (:require [cljam.bcf.reader :as bcf-reader]
            [cljam.bcf.writer :as bcf-writer])
  (:import [cljam.bcf.reader BCFReader]))

;; Reading
;; -------

(def reader bcf-reader/reader)

(defn meta-info
  "Returns meta-information of the BCF from rdr as a map."
  [^BCFReader rdr]
  (-> (.meta-info rdr)
      (update :contig (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))
      (update :filter (fn [xs] (keep (fn [m] (when-not (= (:id m) "PASS") (dissoc m :idx))) xs)))
      (update :info (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))
      (update :format (fn [xs] (map (fn [m] (dissoc m :idx)) xs)))))

(defn header
  "Returns header of the BCF from rdr as a vector including header field strings."
  [^BCFReader rdr]
  (.header rdr))

(def read-variants bcf-reader/read-variants)

;; Writing
;; -------

(def writer bcf-writer/writer)

(def write-variants bcf-writer/write-variants)

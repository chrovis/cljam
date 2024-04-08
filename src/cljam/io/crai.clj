(ns cljam.io.crai
  (:require [cljam.util :as util]
            [cljam.util.intervals :as intervals]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-index
  "Reads a CRAI file `f` and creates an index."
  [f refs]
  (let [refs (vec refs)]
    (with-open [rdr (io/reader (util/compressor-input-stream f))]
      (->> (line-seq rdr)
           (map (fn [line]
                  (let [[^long seq-id ^long start ^long span container-offset slice-offset size]
                        (map #(Long/parseLong %) (str/split line #"\t"))
                        unmapped? (neg? seq-id)]
                    {:chr (if unmapped? "*" (:name (nth refs seq-id)))
                     :start (if unmapped? 0 start)
                     :end (if unmapped? 0 (+ start span))
                     :container-offset container-offset
                     :slice-offset slice-offset
                     :size size})))
           intervals/index-intervals))))

(defn find-overlapping-entries
  "Finds and returns all entries from the index that overlap with the specified
  region."
  [idx chr start end]
  (intervals/find-overlap-intervals idx chr start end))

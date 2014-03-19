(ns cljam.bam-index.core
  "The core of BAM index features."
  (:require [clojure.java.io :as io]
            (cljam.bam-index [common :refer :all]
                             [chunk :as chunk]
                             [reader :as reader])
            [cljam.util.sam-util :refer [ref-id]])
  (:import java.util.BitSet))

(deftype BAMIndex [f bidx lidx])

(defn bam-index [f]
  (let [{:keys [bidx lidx]} (with-open [r (reader/reader f)] (reader/read-all-index! r))]
    (->BAMIndex f bidx lidx)))

(defn bin-index
  [f ref-idx]
  (with-open [r (reader/reader f)]
    (reader/read-bin-index! r ref-idx)))

(defn linear-index
  [f ref-idx]
  (with-open [r (reader/reader f)]
    (reader/read-linear-index! r ref-idx)))

(defn- reg->bins*
  "Returns candidate bins for the specified region as java.util.BitSet."
  [beg end]
  (let [max-pos 0x1FFFFFFF
        beg (if (<= beg 0) 0 (bit-and (dec beg) max-pos))
        end (if (<= end 0) max-pos (bit-and (dec end) max-pos))]
    (if (<= beg end)
      (let [bit-set (BitSet. max-bins)]
        (.set bit-set 0)
        (doseq [[ini shift] [[1 26] [9 23] [73 20] [585 17] [4681 14]]]
          (let [ini* (+ ini (bit-shift-right beg shift))
                end* (+ ini (bit-shift-right end shift))]
           (loop [k ini*]
             (when (<= k end*)
               (.set bit-set k)
               (recur (inc k))))))
        bit-set))))

(def ^:private reg->bins (memoize reg->bins*))

(defn get-spans
  [^BAMIndex bai ref-idx beg end]
  (let [bins ^BitSet (reg->bins beg end)
        bidx (get (.bidx bai) ref-idx) ;; (bin-index bai ref-idx)
        lidx (get (.lidx bai) ref-idx) ;; (linear-index bai ref-idx)
        chunks (->> bidx
                    (filter #(.get bins (:bin %)))
                    (map :chunks)
                    (flatten))
        min-offset (or (first lidx) 0)]
    (->> (chunk/optimize-chunks chunks min-offset)
         (map vals))))

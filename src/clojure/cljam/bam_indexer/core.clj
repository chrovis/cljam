(ns cljam.bam-indexer.core
  (:require [cljam.bam-indexer [common :refer :all]
                               [chunk :as chunk]])
  (:import java.util.BitSet))

(deftype BAMIndex [f]
  java.io.Closeable
  (close [this]
    (.. this f close)))

(defn bam-index [f]
  (->BAMIndex f))

;;; TODO: need bam->BAMIndex

(defn- reg->bins
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

(defn get-spans
  [index ref-idx beg end]
  (let [bins ^BitSet (reg->bins beg end)
        chunks (->> (nth index ref-idx)
                    (:bin-index)
                    (filter #(.get bins (:bin %)))
                    (map :chunks)
                    (flatten))
        min-offset (or (first (:linear-index index)) 0)]
    (->> (chunk/optimize-chunks chunks min-offset)
         (map vals))))

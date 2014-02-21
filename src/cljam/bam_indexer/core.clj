(ns cljam.bam-indexer.core
  (:require [clojure.java.io :as io]
            [cljam.bam-indexer [common :refer :all]
                               [chunk :as chunk]
                               [reader :as reader]])
  (:import java.util.BitSet))

(deftype BAMIndex [f])
;;; TODO: need bam->BAMIndex
(defn bam-index [f]
  (->BAMIndex f))

(defn read-index
  [^BAMIndex bai]
  (with-open [r (reader/reader (.f bai))]
    (reader/read-index r)))

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
  [^BAMIndex bai ref-idx beg end]
  (let [bins ^BitSet (reg->bins beg end)
        index (read-index bai)
        chunks (->> (nth index ref-idx)
                    (:bin-index)
                    (filter #(.get bins (:bin %)))
                    (map :chunks)
                    (flatten))
        min-offset (or (first (:linear-index index)) 0)]
    (->> (chunk/optimize-chunks chunks min-offset)
         (map vals))))

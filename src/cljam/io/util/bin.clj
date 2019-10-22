(ns cljam.io.util.bin
  (:require [cljam.io.util.chunk :as util-chunk]
            [cljam.io.bam-index.writer :as bam-index-writer]))

(defn- reg->bins*
  "Returns candidate bins for the specified region as a vector."
  [^long beg ^long end]
  (let [max-pos 0x1FFFFFFF
        beg (if (<= beg 0) 0 (bit-and (dec beg) max-pos))
        end (if (<= end 0) max-pos (bit-and (dec end) max-pos))]
    (if (<= beg end)
      (loop [bins (transient [0])
             xs [[1 26] [9 23] [73 20] [585 17] [4681 14]]]
        (if-let [[^long ini shift] (first xs)]
          (let [ini* (+ ini (bit-shift-right beg shift))
                end* (+ ini (bit-shift-right end shift))]
            (recur
             (loop [b bins k ini*]
               (if (<= k end*)
                 (recur (conj! b k) (inc k))
                 b))
             (next xs)))
          (persistent! bins))))))

(def ^:private reg->bins (memoize reg->bins*))

(defprotocol IBinaryIndex
  (bidx-ref [this])
  (lidx-ref [this])
  (get-ref-index [this chr]))

(defn get-spans
  [index-data ^long ref-idx ^long beg ^long end]
  (let [bins (reg->bins beg end)
        bidx (get (bidx-ref index-data) ref-idx)
        lidx (get (lidx-ref index-data) ref-idx)
        chunks (into [] (mapcat bidx) bins)
        lin-beg (bam-index-writer/pos->lidx-offset beg)
        min-offset (get lidx lin-beg 0)]
    (->> (util-chunk/optimize-chunks chunks min-offset)
         (map vals))))

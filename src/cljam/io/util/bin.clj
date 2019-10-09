(ns cljam.io.util.bin
  (:refer-clojure :exclude [compare])
  (:require [cljam.io.bam-index [chunk :as bam-index-chunk]
             [writer :as bam-index-writer]])
  (:import [java.io File]
           [java.net MalformedURLException URI URL]
           [cljam.io.bam_index.chunk Chunk]
           [bgzf4j BGZFInputStream BGZFOutputStream]))

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
  (lidx-ref [this]))

(defn get-spans
  [^cljam.io.util.bin.IBinaryIndex index-data ^long ref-idx ^long beg ^long end]
  (let [bins (reg->bins beg end)
        bidx (get (bidx-ref index-data) ref-idx)
        lidx (get (lidx-ref index-data) ref-idx)
        chunks (into [] (comp (map bidx) cat) bins)
        lin-beg (bam-index-writer/pos->lidx-offset beg)
        min-offset (get lidx lin-beg 0)]
    (->> (bam-index-chunk/optimize-chunks chunks min-offset)
         (map vals))))

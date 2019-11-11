(ns cljam.io.util.bin
  (:require [cljam.io.util.chunk :as util-chunk]))

(def ^:const linear-index-shift 14)

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
  (get-chunks [this ref-idx bins])
  (get-min-offset [this ref-idx beg])
  (get-ref-index [this chr]))

(defn pos->lidx-offset
  [^long pos ^long linear-index-shift]
  (bit-shift-right (if (<= pos 0) 0 (dec pos)) linear-index-shift))

(defn get-spans
  "Calculate span information for random access from ndex data such as tabix."
  [index-data ^long ref-idx ^long beg ^long end]
  (let [bins (reg->bins beg end)
        chunks (get-chunks index-data ref-idx bins)
        min-offset (get-min-offset index-data ref-idx beg)]
    (->> (util-chunk/optimize-chunks chunks min-offset)
         (map vals))))

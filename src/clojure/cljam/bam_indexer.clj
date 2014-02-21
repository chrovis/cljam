(ns cljam.bam-indexer
  (:refer-clojure :exclude [compare])
  (:require [cljam.bam :as bam]
            [cljam.io :as io]
            ;; [cljam.util.sam-util :refer [reg->bin]]
            [cljam.util.bgzf-util :as bgzf-util]
            [cljam.bam-indexer.reader :as bai-reader]
            [cljam.bam-indexer.writer :as bai-writer]))

(set! *warn-on-reflection* true)

(defn read-index
  [bai]
  (with-open [r (bai-reader/reader bai)]
    (bai-reader/read-index r)))

(def max-bins 37450)

(defn- reg->bins
  "Returns candidate bins for the specified region as java.util.BitSet."
  [beg end]
  (let [max-pos 0x1FFFFFFF
        beg (if (<= beg 0) 0 (bit-and (dec beg) max-pos))
        end (if (<= end 0) max-pos (bit-and (dec end) max-pos))]
    (if (<= beg end)
      (let [bit-set (java.util.BitSet. max-bins)]
        (.set bit-set 0)
        (doseq [[ini shift] [[1 26] [9 23] [73 20] [585 17] [4681 14]]]
          (let [ini* (+ ini (bit-shift-right beg shift))
                end* (+ ini (bit-shift-right end shift))]
           (loop [k ini*]
             (when (<= k end*)
               (.set bit-set k)
               (recur (inc k))))))
        bit-set))))

(defn- compare
  [chunk1 chunk2]
  (let [ret (Long/signum (- (:beg chunk1) (:beg chunk2)))]
    (if (zero? ret)
      (Long/signum (- (:end chunk1) (:end chunk2)))
      ret)))

(defn- overlap?
  "Returns true if the two chunks overlap."
  [chunk1 chunk2]
  (let [comparison (compare chunk1 chunk2)]
    (or (zero? comparison)
        (let [left (if (neg? comparison) chunk1 chunk2)
              right (if (pos? comparison) chunk1 chunk2)
              left-fp (bgzf-util/get-block-address (:end left))
              right-fp (bgzf-util/get-block-address (:beg right))]
          (or (> left-fp right-fp)
              (and (= left-fp right-fp)
                   (let [left-offset (bgzf-util/get-block-offset (:end left))
                         right-offset (bgzf-util/get-block-offset (:beg right))]
                     (> left-offset right-offset))))))))

(defn- adjacent?
  "Returns true if the two chunks are adjacent."
  [chunk1 chunk2]
  (or (and (= (bgzf-util/get-block-address (:end chunk1))
              (bgzf-util/get-block-address (:beg chunk2)))
           (= (bgzf-util/get-block-offset (:end chunk1))
              (bgzf-util/get-block-offset (:beg chunk2))))
      (and (= (bgzf-util/get-block-address (:beg chunk1))
              (bgzf-util/get-block-address (:end chunk2)))
           (= (bgzf-util/get-block-offset (:beg chunk1))
              (bgzf-util/get-block-offset (:end chunk2))))))

(defn- optimize-chunks
  [chunks min-offset]
  (let [chunks (sort compare chunks)]
    (loop [[f & r] chunks
           last-chunk nil
           ret []]
      (if f
        (cond
         (<= (:end f) min-offset) (recur r last-chunk ret)
         (nil? last-chunk) (recur r f (conj ret f))
         (and (not (overlap? last-chunk f))
              (not (adjacent? last-chunk f))) (recur r f (conj ret f))
         (> (:end f) (:end last-chunk)) (recur r (assoc last-chunk :end (:end f)) ret)
         :else (recur r last-chunk ret))
        ret))))

(defn get-spans
  [index ref-idx beg end]
  (let [bins ^java.util.BitSet (reg->bins beg end)
        chunks (->> (nth index ref-idx)
                    (:bin-index)
                    (filter #(.get bins (:bin %)))
                    (map :chunks)
                    (flatten))
        min-offset (or (first (:linear-index index)) 0)]
    (->> (optimize-chunks chunks min-offset)
         (map vals))))

(defn create-index
  [in-bam out-bai]
  (with-open [r (bam/reader in-bam :ignore-index true)
              w (bai-writer/writer out-bai (io/read-refs r))]
    (bai-writer/write-index w
                            (io/read-alignments r {:depth :pointer}))))

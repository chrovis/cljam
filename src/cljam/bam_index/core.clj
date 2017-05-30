(ns cljam.bam-index.core
  "The core of BAM index features."
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            (cljam.bam-index [common :refer :all]
                             [chunk :as chunk]
                             [reader :as reader]
                             [writer :as writer])
            [cljam.util.sam-util :refer [ref-id]])
  (:import java.util.BitSet
           [java.io DataOutputStream FileOutputStream]
           cljam.bam_index.reader.BAIReader
           cljam.bam_index.writer.BAIWriter))

(deftype BAMIndex [f bidx lidx])

(defn bam-index [f]
  (let [{:keys [bidx lidx]} (with-open [r ^BAIReader (reader/reader f)] (reader/read-all-index! r))]
    (->BAMIndex f bidx lidx)))

;; ## Reading

(defn bin-index
  [f ref-idx]
  (with-open [r ^BAIReader (reader/reader f)]
    (reader/read-bin-index! r ref-idx)))

(defn linear-index
  [f ref-idx]
  (with-open [r ^BAIReader (reader/reader f)]
    (reader/read-linear-index! r ref-idx)))

(defn- reg->bins*
  "Returns candidate bins for the specified region as java.util.BitSet."
  [beg end]
  (let [max-pos 0x1FFFFFFF
        beg (if (<= beg 0) 0 (bit-and (dec beg) max-pos))
        end (if (<= end 0) max-pos (bit-and (dec end) max-pos))]
    (if (<= beg end)
      (let [bit-set (transient [])]
        (conj! bit-set 0)
        (doseq [[ini shift] [[1 26] [9 23] [73 20] [585 17] [4681 14]]]
          (let [ini* (+ ini (bit-shift-right beg shift))
                end* (+ ini (bit-shift-right end shift))]
           (loop [k ini*]
             (when (<= k end*)
               (conj! bit-set k)
               (recur (inc k))))))
        (persistent! bit-set)))))

(def ^:private reg->bins (memoize reg->bins*))

(defn get-spans
  [^BAMIndex bai ref-idx beg end]
  (let [bins (reg->bins beg end)
        bidx (get (.bidx bai) ref-idx)
        lidx (get (.lidx bai) ref-idx)
        chunks (flatten (filter (complement nil?) (map #(get bidx %) bins)))
        min-offset (or (first lidx) 0)]
    (->> (chunk/optimize-chunks chunks min-offset)
         (map vals))))

(defn get-unplaced-spans
  [^BAMIndex bai]
  (if-let [begin (some->>
                  (for [[ref-id bins] (.bidx bai)
                        [bin chunks] bins
                        {:keys [beg end]} chunks]
                    end)
                  seq
                  (reduce max))]
    [[begin Long/MAX_VALUE]]
    []))

;; ## Writing

(defn ^BAIWriter writer
  [f refs]
  (BAIWriter. (DataOutputStream. (FileOutputStream. (cio/file f)))
              refs
              (.getAbsolutePath (cio/file f))))

(defn create-index
  "Creates a BAM index file from the alignments and references data."
  [f alns refs]
  (with-open [w (writer f refs)]
    (try
      (writer/write-index! w alns)
      (catch Exception e (do
                           (cio/delete-file (.f w))
                           (logging/error "Failed to create BAM index")
                           (throw e))))))

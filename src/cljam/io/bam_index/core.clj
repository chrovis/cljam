(ns cljam.io.bam-index.core
  "The core of BAM index features."
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.io.bam-index [common :refer :all]
                                [chunk :as chunk]
                                [reader :as reader]
                                [writer :as writer]]
            [cljam.util :as util])
  (:import [java.io DataOutputStream FileOutputStream]
           cljam.io.bam_index.reader.BAIReader
           cljam.io.bam_index.writer.BAIWriter))

(deftype BAMIndex [url bidx lidx])

(defn bam-index [f]
  (let [{:keys [bidx lidx]} (with-open [r ^BAIReader (reader/reader f)] (reader/read-all-index! r))]
    (->BAMIndex (util/as-url f) bidx lidx)))

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

(defn get-spans
  [^BAMIndex bai ^long ref-idx ^long beg ^long end]
  (let [bins (reg->bins beg end)
        bidx (get (.bidx bai) ref-idx)
        lidx (get (.lidx bai) ref-idx)
        chunks (into [] (comp (map bidx) cat) bins)
        lin-beg (writer/pos->lidx-offset beg)
        min-offset (get lidx lin-beg 0)]
    (->> (chunk/optimize-chunks chunks min-offset)
         (map vals))))

(defn get-unplaced-spans
  [^BAMIndex bai]
  (if-let [begin (some->>
                  (for [[_ bins] (.bidx bai)
                        [_ chunks] bins
                        {:keys [end]} chunks]
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
              (util/as-url f)))

(defn create-index
  "Creates a BAM index file from the alignments and references data."
  [f alns refs]
  (with-open [w (writer f refs)]
    (try
      (writer/write-index! w alns)
      (catch Exception e (do
                           (cio/delete-file (.url w))
                           (logging/error "Failed to create BAM index")
                           (throw e))))))

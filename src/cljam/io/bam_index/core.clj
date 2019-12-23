(ns cljam.io.bam-index.core
  "The core of BAM index features."
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.io.bam-index [reader :as reader]
             [writer :as writer]
             [common :as common]]
            [cljam.util :as util]
            [cljam.io.util.bin :as util-bin])
  (:import [java.io DataOutputStream FileOutputStream]
           cljam.io.bam_index.reader.BAIReader
           cljam.io.bam_index.writer.BAIWriter))

(deftype BAMIndex [url bidx lidx]
  util-bin/IBinningIndex
  (get-chunks [_ ref-idx bins]
    (vec (mapcat (get bidx ref-idx) bins)))
  (get-min-offset [_ ref-idx beg]
    (get (get lidx ref-idx)
         (util-bin/pos->lidx-offset beg common/linear-index-shift) 0))
  (get-depth [_]
    common/linear-index-depth)
  (get-min-shift [_] common/linear-index-shift))

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

(defn get-spans
  [^BAMIndex bai ^long ref-idx ^long beg ^long end]
  (util-bin/get-spans bai ref-idx beg end))


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

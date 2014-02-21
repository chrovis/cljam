(ns cljam.bam-index
  (:require [cljam.bam-indexer [core :as bai-core]]))

(defn read-index [bai]
  (bai-core/read-index bai))

(defn get-spans
  [bai ref-idx beg end]
  (bai-core/get-spans bai ref-idx beg end))

(defn bam-index [f]
  (bai-core/bam-index f))

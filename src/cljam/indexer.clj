(ns cljam.indexer
  (:use [cljam.util :only [reg->bin]])
  (:require [cljam.bam :as bam]))

(def bai-magic "BAI\1")

(defn index [sam]
  nil)

;;; NOTE: should move to cljam.sam...?
(defn compute-bin [sam-alignment]
  (reg->bin (:pos sam-alignment) (bam/get-end sam-alignment)))

(ns cljam.indexer
  (:require (cljam [bam :as bam]
                   [util :refer [reg->bin]])))

(def bai-magic "BAI\1")

(defn index [sam]
  nil)

;;; NOTE: should move to cljam.sam...?
(defn compute-bin [sam-alignment]
  (reg->bin (:pos sam-alignment) (bam/get-end sam-alignment)))

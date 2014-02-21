(ns cljam.bam-indexer
  (:require [cljam.bam :as bam]
            [cljam.io :as io]
            [cljam.bam-indexer [writer :as bai-writer]
                               [core :as bai-core]]))

(defn read-index [bai]
  (bai-core/read-index bai))

(defn get-spans
  [bai ref-idx beg end]
  (bai-core/get-spans bai ref-idx beg end))

(defn create-index
  [in-bam out-bai]
  (with-open [r (bam/reader in-bam :ignore-index true)
              w (bai-writer/writer out-bai (io/read-refs r))]
    (bai-writer/write-index w
                            (io/read-alignments r {:depth :pointer}))))

(defn bam-index [f]
  (bai-core/bam-index f))

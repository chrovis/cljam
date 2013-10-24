(ns cljam.indexer
  (:require (cljam [bam :as bam]
                   [lsb :as lsb]
                   [util :refer [reg->bin]]))
  (:import java.io.DataOutputStream
           ;(net.sf.picard.sam BuildBamIndex BamIndexStats)
           [chrovis.bgzf4j BGZFInputStream BGZFOutputStream]))

(def bai-magic "BAI\1")

(defn index [sam]
  nil)

;;; NOTE: should move to cljam.sam...?
;; (defn compute-bin [sam-alignment]
;;   (reg->bin (:pos sam-alignment) (bam/get-end sam-alignment)))

;;; I/O

(defn spit-bai
  "Opposite of slurp. Opens sam/bam-file with writer, writes sam headers and
  alignments, then closes the sam/bam-file."
  [bai-file sam]
  (with-open [w (DataOutputStream. (BGZFOutputStream. bai-file))]
    (lsb/write-bytes w (.getBytes bai-magic)) ; magic
    ;; TODO
    nil))

;;; Picard interop
;;; HACK: Should not use Picard

;; (defn build-bam-index [in out]
;;   (.. (BuildBamIndex.)
;;       (instanceMain (into-array String [(str "I=" in), (str "O=" out)]))))

;; (defn bam-index-stats [in]
;;   (.. (BamIndexStats.)
;;       (instanceMain (into-array String [(str "I=" in)]))))

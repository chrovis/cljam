(ns cljam.bam.core
  (:require [clojure.java.io :refer [file]]
            [cljam.io]
            (cljam.bam [reader :as reader]
                       [writer :as writer]
                       [index :as index :refer [bam-index]]))
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream IOException EOFException]
           [chrovis.bgzf4j BGZFInputStream BGZFOutputStream]
           [java.nio ByteBuffer ByteOrder]))

;;;
;;; reader
;;;

(defn reader [f {:keys [ignore-index]
                 :or {ignore-index false}}]
  (let [rdr (BGZFInputStream. (file f))
        data-rdr (DataInputStream. rdr)]
    (let [{:keys [header refs]} (reader/load-headers data-rdr)
          index (if ignore-index
                  nil
                  (try (bam-index f header) (catch IOException _ nil)))]
      (cljam.bam.reader.BAMReader. (.getAbsolutePath (file f))
                                   header refs rdr data-rdr index))))

(extend-type cljam.bam.reader.BAMReader
  cljam.io/ISAMReader
  (reader-path [this]
    (.f this))
  (read-header [this]
    (.header this))
  (read-refs [this]
    (.refs this))
  (read-alignments [this {:keys [chr start end depth]
                          :or {chr nil
                               start -1
                               end -1
                               depth :deep}}]
    (if (nil? chr)
      (reader/read-alignments-sequentially* this depth)
      (reader/read-alignments* this chr start end depth)))
  (read-blocks [this]
    (reader/read-blocks-sequentially* this :normal))
  (read-coordinate-blocks [this]
    (reader/read-blocks-sequentially* this :coordinate)))


;;
;; writer
;;

(defn writer [f]
  (cljam.bam.writer.BAMWriter. (.getAbsolutePath (file f))
                               (DataOutputStream. (BGZFOutputStream. (file f)))))

(extend-type cljam.bam.writer.BAMWriter
  cljam.io/ISAMWriter
  (writer-path [this]
    (.f this))
  (write-header [this header]
    (writer/write-header* this header))
  (write-refs [this header]
    (writer/write-refs* this header))
  (write-alignments [this alignments header]
    (writer/write-alignments* this alignments header))
  (write-blocks [this blocks]
    (writer/write-blocks* this blocks))
  (write-coordinate-blocks [this blocks]
    (writer/write-blocks* this blocks)))

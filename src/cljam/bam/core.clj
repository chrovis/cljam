(ns cljam.bam.core
  "The core of BAM features."
  (:require [clojure.java.io :refer [file]]
            [me.raynes.fs :as fs]
            [cljam.io]
            [cljam.lsb :as lsb]
            (cljam.bam [common :refer [bam-magic]]
                       [reader :as reader]
                       [writer :as writer])
            [cljam.bam-index :as bai])
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream IOException]
           [bgzf4j BGZFInputStream BGZFOutputStream]
           cljam.bam.reader.BAMReader
           cljam.bam.writer.BAMWriter))

;; Reading
;; -------

(defn- bam-index [f & {:keys [ignore]
                       :or {ignore false}}]
  (if-not ignore
    (let [bai-f (str f ".bai")]
      (if (fs/exists? bai-f)
        (bai/bam-index bai-f)
        (throw (IOException. "Could not find BAM Index file"))))))

(defn ^BAMReader reader [f {:keys [ignore-index]
                            :or {ignore-index false}}]
  (let [rdr (BGZFInputStream. (file f))
        data-rdr (DataInputStream. rdr)]
    (when-not (Arrays/equals ^bytes (lsb/read-bytes data-rdr 4) (.getBytes ^String bam-magic))
      (throw (IOException. "Invalid BAM file")))
    (let [{:keys [header refs]} (reader/load-headers data-rdr)
          index-delay (delay (bam-index f :ignore ignore-index))]
      (BAMReader. (.getAbsolutePath (file f))
                  header refs rdr data-rdr index-delay))))

(extend-type BAMReader
  cljam.io/ISAMReader
  (reader-path [this]
    (.f this))
  (read-header [this]
    (.header this))
  (read-refs [this]
    (.refs this))
  (read-alignments
    ([this]
       (reader/read-alignments-sequentially* this :deep))
    ([this {:keys [chr start end depth]
            :or {chr nil
                 start -1 end -1
                 depth :deep}}]
       (if (nil? chr)
         (reader/read-alignments-sequentially* this depth)
         (reader/read-alignments* this chr start end depth))))
  (read-blocks
    ([this]
       (reader/read-blocks-sequentially* this :normal))
    ([this {:keys [chr start end mode]
            :or {chr nil
                 start -1 end -1
                 mode :normal}}]
     (if (nil? chr)
       (reader/read-blocks-sequentially* this mode)
       (reader/read-blocks* this chr start end)))))

;; Writing
;; -------

(defn ^BAMWriter writer [f]
  (BAMWriter. (.getAbsolutePath (file f))
              (DataOutputStream. (BGZFOutputStream. (file f)))))

(extend-type BAMWriter
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
    (writer/write-blocks* this blocks)))

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
  (when-not ignore
    (let [bai-f (str f ".bai")]
      (when (fs/exists? bai-f)
        (bai/bam-index bai-f)))))

(defn ^BAMReader reader [f {:keys [ignore-index]
                            :or {ignore-index false}}]
  (let [rdr (BGZFInputStream. (file f))
        data-rdr (DataInputStream. rdr)]
    (when-not (Arrays/equals ^bytes (lsb/read-bytes data-rdr 4) (.getBytes ^String bam-magic))
      (throw (IOException. "Invalid BAM file")))
    (let [{:keys [header refs]} (reader/load-headers data-rdr)
          index-delay (delay (bam-index f :ignore ignore-index))]
      (BAMReader. (.getAbsolutePath (file f))
                  header refs rdr data-rdr index-delay (.getFilePointer rdr)))))

(defn ^BAMReader clone-reader
  "Clones bam reader sharing persistent objects."
  [^BAMReader rdr]
  (let [bgzf-rdr (BGZFInputStream. (file (.f rdr)))
        data-rdr (DataInputStream. bgzf-rdr)]
    (.seek bgzf-rdr (.start-pos rdr))
    (BAMReader. (.f rdr) (.header rdr) (.refs rdr) bgzf-rdr data-rdr (.index-delay rdr) (.start-pos rdr))))

;; Writing
;; -------

(defn ^BAMWriter writer [f]
  (BAMWriter. (.getAbsolutePath (file f))
              (DataOutputStream. (BGZFOutputStream. (file f)))))

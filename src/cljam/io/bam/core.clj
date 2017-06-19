(ns cljam.io.bam.core
  "The core of BAM features."
  (:require [clojure.java.io :as cio]
            [cljam.io.bam [common :refer [bam-magic]]
                          [reader :as reader]
                          [writer :as writer]]
            [cljam.io.bam-index :as bai]
            [cljam.io.util.lsb :as lsb])
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream IOException]
           [bgzf4j BGZFInputStream BGZFOutputStream]
           cljam.io.bam.reader.BAMReader
           cljam.io.bam.writer.BAMWriter))

;; Reading
;; -------

(defn- bam-index [f & {:keys [ignore]
                       :or {ignore false}}]
  (if-not ignore
    (let [bai-f (str f ".bai")]
      (if (.isFile (cio/file bai-f))
        (bai/bam-index bai-f)
        (throw (IOException. "Could not find BAM Index file"))))))

(defn ^BAMReader reader [f {:keys [ignore-index]
                            :or {ignore-index false}}]
  (let [rdr (BGZFInputStream. (cio/file f))
        data-rdr (DataInputStream. rdr)]
    (when-not (Arrays/equals ^bytes (lsb/read-bytes data-rdr 4) (.getBytes ^String bam-magic))
      (throw (IOException. "Invalid BAM file")))
    (let [{:keys [header refs]} (reader/load-headers data-rdr)
          index-delay (delay (bam-index f :ignore ignore-index))]
      (BAMReader. (.getAbsolutePath (cio/file f))
                  header refs rdr data-rdr index-delay (.getFilePointer rdr)))))

(defn ^BAMReader clone-reader
  "Clones bam reader sharing persistent objects."
  [^BAMReader rdr]
  (let [bgzf-rdr (BGZFInputStream. (cio/file (.f rdr)))
        data-rdr (DataInputStream. bgzf-rdr)]
    (.seek bgzf-rdr (.start-pos rdr))
    (BAMReader. (.f rdr) (.header rdr) (.refs rdr) bgzf-rdr data-rdr (.index-delay rdr) (.start-pos rdr))))

;; Writing
;; -------

(defn ^BAMWriter writer [f]
  (BAMWriter. (.getAbsolutePath (cio/file f))
              (DataOutputStream. (BGZFOutputStream. (cio/file f)))))

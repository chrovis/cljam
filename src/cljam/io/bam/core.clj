(ns cljam.io.bam.core
  "The core of BAM features."
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.io.bam [common :refer [bam-magic]]
                          [reader :as reader]
                          [writer :as writer]]
            [cljam.io.bam-index :as bai]
            [cljam.io.util.lsb :as lsb])
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream IOException FileNotFoundException]
           [bgzf4j BGZFInputStream BGZFOutputStream]
           cljam.io.bam.reader.BAMReader
           cljam.io.bam.writer.BAMWriter))

;; Reading
;; -------

(defn- bam-index
  "Load an index file (BAI) for the given BAM file."
  [bam-path {:keys [ignore]}]
  (let [bai-path (->> ["$1.bai" ".bai" "$1.BAI" ".BAI"]
                      (eduction
                       (comp
                        (map #(cstr/replace bam-path #"(?i)(\.bam)$" %))
                        (filter #(.isFile (cio/file %)))))
                      first)]
    (if (and bai-path (not ignore))
      (bai/bam-index bai-path)
      (throw (FileNotFoundException. (str "Could not find BAM Index file for " bam-path))))))

(defn ^BAMReader reader
  "Creates a `cljam.io.bam.BAMReader` instancer for the given path.
  Pass {:ignore-index true} to force to ignore BAI file."
  [f {:keys [ignore-index]}]
  (let [rdr (BGZFInputStream. (cio/file f))
        data-rdr (DataInputStream. rdr)]
    (when-not (Arrays/equals ^bytes (lsb/read-bytes data-rdr 4) (.getBytes ^String bam-magic))
      (throw (IOException. "Invalid BAM file")))
    (let [{:keys [header refs]} (reader/load-headers data-rdr)
          index-delay (delay (bam-index f {:ignore ignore-index}))]
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

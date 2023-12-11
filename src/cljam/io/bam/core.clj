(ns cljam.io.bam.core
  "The core of BAM features."
  (:require [clojure.string :as cstr]
            [cljam.io.bam [common :as common]
             [reader :as reader]
             [writer :as writer]]
            [cljam.io.bam-index :as bai]
            [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.lsb.data-io :as lsb]
            [cljam.util :as util])
  (:import java.util.Arrays
           [java.io DataInputStream DataOutputStream IOException FileNotFoundException]
           cljam.io.bam.reader.BAMReader
           cljam.io.bam.writer.BAMWriter))

;; Reading
;; -------

(defn- bam-index
  "Loads an index file (BAI) for the given BAM URL."
  [bam-url]
  (or (->> ["$1.bai" ".bai" "$1.BAI" ".BAI"]
           (eduction
            (comp
             (map #(cstr/replace (str bam-url) #"(?i)(\.bam)$" %))
             (map util/as-url)
             (keep #(try (bai/bam-index %) (catch FileNotFoundException _)))))
           first)
      (throw (FileNotFoundException.
              (str "Could not find BAM Index file for " bam-url)))))

(defn reader
  "Creates a `cljam.io.bam.BAMReader` instance for the given file."
  ^BAMReader
  [f]
  (let [rdr (bgzf/bgzf-input-stream f)
        data-rdr (DataInputStream. rdr)]
    (when-not (Arrays/equals ^bytes (lsb/read-bytes data-rdr 4) (.getBytes ^String common/bam-magic))
      (throw (IOException. "Invalid BAM file")))
    (let [{:keys [header refs]} (reader/load-headers data-rdr)
          index-delay (delay (bam-index (util/as-url f)))]
      (BAMReader. (util/as-url f)
                  header refs rdr data-rdr index-delay (.getFilePointer rdr)))))

(defn clone-reader
  "Clones bam reader sharing persistent objects."
  ^BAMReader
  [^BAMReader rdr]
  (let [bgzf-rdr (bgzf/bgzf-input-stream (.url rdr))
        data-rdr (DataInputStream. bgzf-rdr)]
    (.seek bgzf-rdr (.start-pos rdr))
    (BAMReader. (.url rdr) (.header rdr) (.refs rdr) bgzf-rdr data-rdr (.index-delay rdr) (.start-pos rdr))))

;; Writing
;; -------

(defn writer
  "Returns an open `cljam.io.bam.BAMWriter` of `f`. Should be used inside with-open
  to ensure the writer is properly closed."
  (^BAMWriter [f] (writer f false))
  (^BAMWriter [f create-index?]
   (let [index (if create-index? {:no-coordinate-alns 0} false)
         w (bgzf/bgzf-output-stream f)]
     (BAMWriter. (util/as-url f)
                 w
                 (DataOutputStream. w)
                 (atom nil)
                 (atom index)))))

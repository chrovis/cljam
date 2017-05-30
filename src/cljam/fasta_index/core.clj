(ns cljam.fasta-index.core
  "The core of FASTA index features."
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.fasta-index.writer :as writer]
            [cljam.fasta-index.reader :as reader]
            [cljam.util :as util]))

;;;; Writing

(defn writer
  [f]
  (cljam.fasta_index.writer.FAIWriter.
   (cio/writer f)
   (.getAbsolutePath (cio/file f))))

(defn create-index
  "Creates a FASTA index file from the sequences."
  [in-fa out-fai]
  (with-open [r (cio/reader (util/compressor-input-stream in-fa))
              w ^cljam.fasta_index.writer.FAIWriter (writer out-fai)]
    (try
      (writer/write-index! r w)
      (catch Exception e (do
                           (cio/delete-file (.f w))
                           (logging/error "Failed to create FASTA index")
                           (throw e))))))

;;;; Reading

(defn reader
  [f]
  (cljam.fasta_index.reader.FAIReader.
   (with-open [rdr (cio/reader f)]
     (reader/parse-fai rdr))
   (.getAbsolutePath (cio/file f))))

(defn get-header
  [^cljam.fasta_index.reader.FAIReader fai name]
  (merge {:name name}
         (get (.indices fai) name nil)))

(defn get-headers
  [^cljam.fasta_index.reader.FAIReader fai]
  (vec
   (sort
    #(compare (:offset %1) (:offset %2))
    (map (fn [[k v]]
           {:name k
            :desc ""
            :offset (:offset v)})
         (.indices fai)))))

(defn get-indices
  [^cljam.fasta_index.reader.FAIReader fai]
  (vec
   (sort
    #(compare (:offset %1) (:offset %2))
    (map (fn [[k v]] (merge {:name k} v))
         (.indices fai)))))

(defn get-span
  "Calculate byte spans for FASTA file"
  [^cljam.fasta_index.reader.FAIReader fai name start end]
  (let [start (max 0 start)
        end (max 0 end)]
    (when-let [index (get (.indices fai) name nil)]
      (let [start (min (:len index) start)
            end (min (:len index) end)
            proj (fn [pos]
                   (+ (:offset index)
                      (+ (* (quot pos (:line-blen index))
                            (:line-len index))
                         (rem pos (:line-blen index)))))]
        (when (< start end)
          [(proj start) (proj end)])))))

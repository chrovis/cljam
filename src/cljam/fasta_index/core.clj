(ns cljam.fasta-index.core
  "The core of FASTA index features."
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as logging]
            [me.raynes.fs :as fs]
            [cljam.fasta-index.writer :as writer]
            [cljam.fasta-index.reader :as reader]))

;;;; Writing

(defn writer
  [f]
  (cljam.fasta_index.writer.FAIWriter.
   (io/writer f)
   (.getAbsolutePath (io/file f))))

(defn create-index
  "Creates a FASTA index file from the sequences."
  [f headers sequences]
  (with-open [w ^cljam.fasta_index.writer.FAIWriter (writer f)]
    (try
      (writer/write-index! w headers sequences)
      (catch Exception e (do
                           (fs/delete (.f w))
                           (logging/error "Failed to create FASTA index")
                           (throw e))))))

;;;; Reading

(defn reader
  [f]
  (cljam.fasta_index.reader.FAIReader.
   (with-open [rdr (io/reader f)]
     (reader/parse-fai rdr))
   (.getAbsolutePath (io/file f))))

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

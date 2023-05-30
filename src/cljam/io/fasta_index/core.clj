(ns cljam.io.fasta-index.core
  "The core of FASTA index features."
  (:require [clojure.java.io :as cio]
            [clojure.tools.logging :as logging]
            [cljam.io.fasta-index.writer :as writer]
            [cljam.io.fasta-index.reader :as reader]
            [cljam.util :as util])
  (:import cljam.io.fasta_index.reader.FAIReader
           cljam.io.fasta_index.writer.FAIWriter))

;;;; Writing

(defn writer
  [f]
  (FAIWriter.
   (cio/writer f)
   (util/as-url f)))

(defn create-index
  "Creates a FASTA index file from the sequences."
  [in-fa out-fai]
  (with-open [r (cio/reader (util/compressor-input-stream in-fa))
              w ^FAIWriter (writer out-fai)]
    (try
      (writer/write-index! r w)
      (catch Exception e
        (cio/delete-file (.url w))
        (logging/error "Failed to create FASTA index")
        (throw e)))))

;;;; Reading

(defn reader
  [f]
  (FAIReader.
   (with-open [rdr (cio/reader f)]
     (reader/parse-fai rdr))
   (util/as-url f)))

(defn get-header
  [^FAIReader fai name]
  (merge {:name name}
         (get (.indices fai) name nil)))

(defn get-headers
  [^FAIReader fai]
  (vec
   (sort
    #(compare (:offset %1) (:offset %2))
    (map (fn [[k v]]
           {:name k
            :desc ""
            :offset (:offset v)})
         (.indices fai)))))

(defn get-indices
  [^FAIReader fai]
  (vec
   (sort
    #(compare (:offset %1) (:offset %2))
    (map (fn [[k v]] (merge {:name k} v))
         (.indices fai)))))

(defn get-span
  "Calculate byte spans for FASTA file"
  [^FAIReader fai name ^long start ^long end]
  (let [start (max 0 start)
        end (max 0 end)]
    (when-let [{^long index-offset :offset
                ^long index-len :len
                ^long index-line-len :line-len
                ^long index-line-blen :line-blen}
               (get (.indices fai) name nil)]
      (let [start (min index-len start)
            end (min index-len end)
            proj (fn [^long pos]
                   (+ index-offset
                      (+ (* (quot pos index-line-blen)
                            index-line-len)
                         (rem pos index-line-blen))))]
        (when (< start end)
          [(proj start) (proj end)])))))

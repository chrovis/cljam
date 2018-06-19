(ns cljam.io.fasta.writer
  (:require [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.util :as util]
            [cljam.io.protocols :as protocols])
  (:import [java.io Closeable BufferedWriter]))

(declare write-sequences)

(deftype FASTAWriter [url ^int cols writer index-writer curr-offset]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this))
    (when-let [iw (.index-writer this)]
      (.close ^Closeable iw)))
  protocols/IWriter
  (writer-url [this]
    (.url this))
  protocols/ISequenceWriter
  (write-sequences [this seqs]
    (write-sequences this seqs)))

(defn writer [f {:keys [cols create-index?]
                 :or {cols 80 create-index? true}}]
  (let [abs-f (.getAbsolutePath (cio/file f))
        writer (cio/writer (util/compressor-output-stream abs-f))
        index-writer (when create-index? (cio/writer (str abs-f ".fai")))]
    (FASTAWriter. (util/as-url abs-f) cols writer index-writer (volatile! 0))))

(defn- write-name
  [^FASTAWriter w ^String n]
  (let [wtr ^BufferedWriter (.writer w)]
    (.write wtr (int \>))
    (.write wtr n)
    (.newLine wtr)
    (+ 1 (.length n) (.length (System/lineSeparator)))))

(defn- write-seq-str
  [^FASTAWriter w ^String s]
  (let [wtr ^BufferedWriter (.writer w)
        l ^int (.length s)
        c (.cols w)
        n (quot (dec (+ l c)) c)]
    (dotimes [i n]
      (.write wtr s (int (* i c)) (int (if (= i (dec n)) (- l (* c i)) c)))
      (.newLine wtr))
    [l (+ l (* n (.length (System/lineSeparator))))]))

(defn- write-seq
  [^FASTAWriter w col]
  (let [wtr ^BufferedWriter (.writer w)
        nl-size (.length (System/lineSeparator))]
    (loop [seq-len 0 written 0 xs (partition-all (.cols w) col)]
      (if-let [x (first xs)]
        (do (.write wtr (cstr/join x))
            (.newLine wtr)
            (recur (+ seq-len (count x)) (+ written nl-size (count x)) (next xs)))
        [seq-len written]))))

(defn- write-sequence
  [^FASTAWriter w {:keys [name rname seq sequence]}]
  (let [chr-name (or name rname)
        seq-data (or seq sequence)
        name-bytes (write-name w chr-name)
        [seq-len seq-bytes] (if (string? seq-data)
                              (write-seq-str w seq-data)
                              (write-seq w seq-data))]
    (when-let [iwtr ^BufferedWriter (.index-writer w)]
      (let [c (Math/min (.cols w) (int seq-len))
            offset (+ @(.curr-offset w) name-bytes)]
        (->> [chr-name seq-len offset c (+ c (.length (System/lineSeparator)))]
             (cstr/join \tab)
             (.write iwtr))
        (.newLine iwtr)
        (vswap! (.curr-offset w) + name-bytes seq-bytes)))))

(defn write-sequences
  [w xs]
  (doseq [x xs]
    (write-sequence w x)))

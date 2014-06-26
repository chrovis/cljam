(ns cljam.fasta.reader
  (:refer-clojure :exclude [read slurp])
  (:require [clojure.string :as cstr]
            [cljam.util :refer [graph? space?]]
            [cljam.fasta-index.core :as fasta-index])
  (:import java.io.RandomAccessFile))

;; FASTAReader
;; -----------

(deftype FASTAReader [reader f index]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.reader this))))

;; Reading
;; -------

(defn- header-line?
  [line]
  (= (first line) \>))

(def ^:private vertial-bar (char 0x7c))

(defn- vertial-bar?
  [c]
  (= c vertial-bar))

(defn- parse-header-line
  [line]
  (let [line (subs line 1)]
    {:name (->> line
                (take-while #(and ((complement space?) %)
                                  ((complement vertial-bar?) %)))
                (apply str))
     :desc (->> line
                (drop-while #(and ((complement space?) %)
                                  ((complement vertial-bar?) %)))
                (drop 1)
                (apply str))}))

(defn- read* [line ^RandomAccessFile rdr]
  (loop [line line
         ret {}]
    (if-not (nil? line)
      (if (= (first line) \>)
        (if (seq ret)
          (cons (assoc ret :len (count (filter (partial not= \space) (:seq ret))))
                (lazy-seq (read* line rdr)))
          (let [ref (subs line 1)
                offset (.getFilePointer rdr)]
            (recur (.readLine rdr) (assoc ret :rname ref :offset offset))))
        (let [ret' (if (:line-len ret)
                     (update-in ret [:seq] str line)
                     (assoc ret
                       :seq line
                       :line-len (inc (count line))
                       :line-blen (count (filter graph? line))))]
          (recur (.readLine rdr) ret')))
      (cons (assoc ret :len (count (filter (partial not= \space) (:seq ret))))
            nil))))

(defn load-headers
  [^RandomAccessFile rdr]
  (.seek rdr 0)
  (loop [line (.readLine rdr), headers []]
    (if line
      (if (header-line? line)
        (let [offset (.getFilePointer rdr)]
          (recur (.readLine rdr) (conj headers (merge (parse-header-line line)
                                                      {:offset offset}))))
        (recur (.readLine rdr) headers))
      headers)))

(defn- read-sequence*
  [^FASTAReader rdr name]
  (let [reader ^RandomAccessFile (.reader rdr)
        line (.readLine reader)]
    (if line
      (if-not (header-line? line)
        {:name name, :sequence line}
        (:name (parse-header-line line))))))

(defn read-sequences
  "Reads sequences by line, returning the line-separated sequences
  as lazy sequence."
  [^FASTAReader rdr]
  (.seek (.reader rdr) 0)
  (let [read-fn (fn read-fn* [^FASTAReader rdr name]
                  (let [s (read-sequence* rdr name)]
                    (cond
                     (string? s) (read-fn* rdr s)
                     (map? s) (cons s (lazy-seq (read-fn* rdr name))))))]
    (read-fn rdr nil)))

(def ^:private newline-character (char 10))
(def ^:private return-character (char 13))

(defn- newline*?
  [c]
  (or (= c newline-character)
      (= c return-character)))

(def newline? (memoize newline*?))

(defn- read-sequence-with-offset
  [rdr offset-start offset-end]
  (let [len (- offset-end offset-start)
        ba (byte-array len)
        r (.reader rdr)
        buf (StringBuffer. len)]
    (.seek r offset-start)
    (.read r ba 0 len)
    (doseq [b ^byte ba]
      (let [c (char ^byte b)]
        (when-not (newline? c)
          (.append buf (Character/toUpperCase c)))))
    (.toString buf)))

(defn read-whole-sequence
  [^FASTAReader rdr name]
  (when-let [fai (.index rdr)]
    (let [header (fasta-index/get-header fai name)
          [offset-start offset-end] (fasta-index/get-span fai name 0 (:len header))]
      (read-sequence-with-offset rdr offset-start offset-end))))

(defn read-sequence
  [^FASTAReader rdr name start end]
  (when-let [fai (.index rdr)]
    (let [[offset-start offset-end] (fasta-index/get-span fai name start end)]
      (read-sequence-with-offset rdr offset-start offset-end))))

(defn read
  "Reads FASTA sequence data, returning its information as a lazy sequence."
  [^FASTAReader rdr]
  (let [r (.reader rdr)]
    (read* (.readLine r) r)))

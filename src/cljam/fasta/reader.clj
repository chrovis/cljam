(ns cljam.fasta.reader
  (:refer-clojure :exclude [read slurp])
  (:require [cljam.util :refer [graph?]])
  (:import java.io.RandomAccessFile))

;; FASTAReader
;; -----------

(deftype FASTAReader [reader f headers]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.reader this))))

;; Reading
;; -------

(defn- header-line?
  [line]
  (= (first line) \>))

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
  (loop [line (.readLine rdr), headers []]
    (if line
      (if (header-line? line)
        (let [offset (.getFilePointer rdr)]
          (recur (.readLine rdr) (conj headers {:name (subs line 1)
                                                :offset offset})))
        (recur (.readLine rdr) headers))
      headers)))

(defn- read-sequence*
  [^FASTAReader rdr name]
  (let [reader ^RandomAccessFile (.reader rdr)
        line (.readLine reader)]
    (if line
      (if-not (header-line? line)
        {:name name, :sequence line}
        (subs line 1)))))

(defn read-sequences
  "Reads sequences by line, returning the line-separated sequences
  as lazy sequence."
  [^FASTAReader rdr]
  (let [reader ^RandomAccessFile (.reader rdr)
        read-fn (fn read-fn* [^FASTAReader rdr name]
                  (let [s (read-sequence* rdr name)]
                    (cond
                     (string? s) (read-fn* rdr s)
                     (map? s) (cons s (lazy-seq (read-fn* rdr name))))))]
    (read-fn rdr nil)))

(defn read
  "Reads FASTA sequence data, returning its information as a lazy sequence."
  [^FASTAReader rdr]
  (let [r (.reader rdr)]
    (read* (.readLine r) r)))

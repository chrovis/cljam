(ns cljam.fasta.reader
  (:refer-clojure :exclude [read slurp])
  (:require [cljam.util :refer [graph?]])
  (:import java.io.RandomAccessFile))

;; FASTAReader
;; -----------

(deftype FASTAReader [reader f]
  java.io.Closeable
  (close [this]
    (.close ^java.io.Closeable (.reader this))))

;; Reading
;; -------

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

;; Public
;; ------

(defn read
  "Reads FASTA sequence data, returning its information as a lazy sequence."
  [^FASTAReader rdr]
  (let [r (.reader rdr)]
    (read* (.readLine r) r)))

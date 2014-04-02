(ns cljam.fasta
  "Alpha - subject to change.
  Reader of a FASTA format file."
  (:refer-clojure :exclude [read slurp])
  (:import [java.io RandomAccessFile Closeable]))

;;;
;;; FASTAReader
;;;

(deftype FASTAReader [reader f]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this))))

;;;
;;; Read
;;;

(defn- read* [line ^RandomAccessFile rdr]
  (loop [line line
         ret {}]
    (if-not (nil? line)
      (if (= (first line) \>)
        (if (seq ret)
          (cons (assoc ret :blen (count (filter (partial not= \space) (:seq ret))))
                (lazy-seq (read* line rdr)))
          (let [ref (subs line 1)
                offset (.getFilePointer rdr)]
            (recur (.readLine rdr) (assoc ret :rname ref :offset offset))))
        (recur (.readLine rdr) (update-in ret [:seq] str line)))
      (cons (assoc ret :blen (count (filter (partial not= \space) (:seq ret))))
            nil))))

;;;
;;; Public
;;;

(defn reader
  "Creates a reader on a FASTA file."
  [f]
  (->FASTAReader (RandomAccessFile. f "r") f))

(defn read
  "Reads FASTA sequence data, returning its information as a lazy sequence."
  [^FASTAReader rdr]
  (let [r (.reader rdr)]
    (read* (.readLine r) r)))

(defn slurp
  "Opens a reader on a FASTA file and reads all its contents, returning
  a sequence about the data."
  [f]
  (with-open [r (reader f)]
    (doall (read r))))

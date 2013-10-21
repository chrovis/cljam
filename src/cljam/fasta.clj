(ns cljam.fasta
  (:refer-clojure :exclude [read slurp])
  (:import java.io.RandomAccessFile))

(defn reader
  "Creates a reader on a FASTA file."
  [f]
  (RandomAccessFile. f "r"))

(defn- read* [line rdr]
  (loop [line line
         ret {}]
    (if-not (nil? line)
      (if (= (first line) \>)
        (if (seq ret)
          (cons (assoc ret :blen (count (filter (partial not= \space) (:seq ret))))
                (lazy-seq (read* line rdr)))
          (let [ref (subs line 1)
                offset (.getFilePointer rdr)]
            (recur (.readLine rdr) (assoc ret :ref ref :offset offset))))
        (recur (.readLine rdr) (update-in ret [:seq] str line)))
      (cons (assoc ret :blen (count (filter (partial not= \space) (:seq ret))))
            nil))))

(defn read
  "Reads FASTA sequence data, returning its information as a lazy sequence."
  [rdr]
  (read* (.readLine rdr) rdr))

(defn slurp
  "Opens a reader on a FASTA file and reads all its contents, returning
  a sequence about the data."
  [f]
  (with-open [r (reader f)]
    (doall (read r))))

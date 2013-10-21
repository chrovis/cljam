(ns cljam.fasta
  (:refer-clojure :exclude [read slurp])
  (:require [clojure.java.io :refer [file writer]]
            [cljam.sam :as sam]
            [cljam.util :refer [string->bytes upper-case]])
  (:import java.io.RandomAccessFile
           java.security.MessageDigest))

(defn reader [f]
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

(defn read [rdr]
  (read* (.readLine rdr) rdr))

(defn slurp
  "Opens a reader on a fasta-file and reads all its contents, returning a map
  about the data."
  [f]
  (with-open [r (reader f)]
    (doall (read r))))

(defn- md5-hash
  [^bytes b]
  (let [md5 (MessageDigest/getInstance "MD5")]
    (.reset md5)
    (.update md5 b)
    (let [s (.toString (BigInteger. 1 (.digest md5)) 16)]
      (if-not (= (count s) 32)
        (str (subs "00000000000000000000000000000000" 0 (- 32 (count s))) s)
        s))))

(defn- make-hash [seq]
  (let [bases (string->bytes seq)]
    (loop [i 0]
      (when (< i (count bases))
        (aset bases i ^byte (upper-case (nth bases i)))
        (recur (inc i))))
    (md5-hash bases)))

(defn- write-sq [wrtr ref seq ur]
  (let [blen (count (filter (partial not= \space) seq))
        m5 (make-hash seq)]
    (.write wrtr (str "@SQ\tSN:" ref "\tLN:" blen "\tUR:" ur "\tM5:" m5)))
  (.newLine wrtr))

(defn create-dict
  "fasta -> dict"
  [fasta out-dict]
  (let [ur (.toString (.toURI (file fasta)))]
   (with-open [r (reader fasta)
               w (writer out-dict)]
     (.write w (str "@HD\tVN:" sam/version "\tSO:unsorted"))
     (.newLine w)
     (doseq [sq (read r)]
       (write-sq w (:ref sq) (:seq sq) ur)))))

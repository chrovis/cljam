(ns cljam.fasta
  (:refer-clojure :exclude [slurp])
  (:require [clojure.java.io :refer [file reader writer]]
            [cljam.util :refer [string->bytes upper-case]])
  (:import java.io.RandomAccessFile
           java.security.MessageDigest))

(defn slurp
  "Opens a reader on a fasta-file and reads all its contents, returning a map
  about the data."
  [f]
  (with-open [r (RandomAccessFile. f "r")]
    (loop [fa []
           line (.readLine r)]
      (if (nil? line)
        fa
        (if (= (first line) \>)
          (let [ref    (subs line 1)
                offset (.getFilePointer r)
                seq    (.readLine r)
                blen   (count (filter (partial not= \space) seq))]
            (recur (conj fa {:ref ref, :offset offset, :seq seq, :blen blen})
                   (.readLine r)))
          (recur fa (.readLine r)))))))

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
     (.write w "@HD\tVN:1.4\tSO:unsorted")
     (.newLine w)
     (loop [line (.readLine r)
            ref nil
            seq nil]
       (if (nil? line)
         (when-not (nil? ref)
           (write-sq w ref seq ur))
         (if (= (first line) \>)
           (do
             (when-not (nil? ref)
               (write-sq w ref seq ur))
             (recur (.readLine r) (subs line 1) nil))
           (recur (.readLine r) ref (str seq line))))))))

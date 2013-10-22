(ns cljam.dict
  (:require [clojure.java.io :refer [file writer]]
            [cljam.sam :as sam]
            [cljam.fasta :as fasta]
            [cljam.util :refer [string->bytes upper-case]])
  (:import [java.io BufferedWriter RandomAccessFile]
           java.security.MessageDigest))

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
        (aset ^bytes bases i ^byte (upper-case (nth bases i)))
        (recur (inc i))))
    (md5-hash bases)))

(defn- write-header
  [^BufferedWriter wrtr]
  (.write wrtr (str "@HD\tVN:" sam/version "\tSO:unsorted"))
  (.newLine wrtr))

(defn- write-sq
  [^BufferedWriter wrtr ref seq ur]
  (let [blen (count (filter (partial not= \space) seq))
        m5 (make-hash seq)]
    (.write wrtr (str "@SQ\tSN:" ref "\tLN:" blen "\tUR:" ur "\tM5:" m5)))
  (.newLine wrtr))

(defn create-dict
  "Creates a FASTA sequence dictionary file (.dict) from the specified a FASTA
  file (.fasta/fa)."
  [fasta out-dict]
  (let [ur (.toString (.toURI (file fasta)))]
   (with-open [r ^RandomAccessFile (fasta/reader fasta)
               w ^BufferedWriter (writer out-dict)]
     (write-header w)
     (doseq [sq (fasta/read r)]
       (write-sq w (:ref sq) (:seq sq) ur)))))

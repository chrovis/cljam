(ns cljam.io.tabix
  "Alpha - subject to change.
  Reader of a TABIX format file."
  (:require [clojure.java.io :as cio]
            [cljam.io.util.lsb :as lsb])
  (:import java.util.Arrays
           [java.io DataInputStream IOException]
           bgzf4j.BGZFInputStream))

(def tabix-magic "TBI\1")

(defn- read-seq
  [buf len]
  (loop [i 0, j 0, seq* []]
    (if (< i len)
     (if (zero? (nth buf i))
       (let [ba-len (- i j)
             ba (byte-array ba-len)]
         (System/arraycopy buf j ba 0 ba-len)
         (recur (inc i) (inc i) (conj seq* (String. ba))))
       (recur (inc i) j seq*))
     seq*)))

(defn- read-chunk
  [^DataInputStream rdr]
  {:beg (lsb/read-long rdr)
   :end (lsb/read-long rdr)})

(defn- read-bin
  [^DataInputStream rdr]
  (let [bin (lsb/read-int rdr)
        n-chunk (lsb/read-int rdr)]
    {:bin bin
     :chunks (doall (map (fn [_] (read-chunk rdr)) (range n-chunk)))}))

(defn- read-index*
  [^DataInputStream rdr]
  (when-not (Arrays/equals ^bytes (lsb/read-bytes rdr 4) (.getBytes ^String tabix-magic))
    (throw (IOException. "Invalid TABIX file")))
  (let [n-seq  (lsb/read-int rdr)
        preset (lsb/read-int rdr)
        sc     (lsb/read-int rdr)
        bc     (lsb/read-int rdr)
        ec     (lsb/read-int rdr)
        meta   (lsb/read-int rdr)
        skip   (lsb/read-int rdr)
        len    (lsb/read-int rdr)
        buf    (lsb/read-bytes rdr len)
        seq    (read-seq buf len)
        index  (loop [i 0
                      bin-index []
                      linear-index []]
                 (if (< i n-seq)
                   (let [n-bin (lsb/read-int rdr)
                         new-bin-index (doall (map (fn [_] (read-bin rdr)) (range n-bin)))
                         n-linear-index (lsb/read-int rdr)
                         new-linear-index (doall (map (fn [_] (lsb/read-long rdr)) (range n-linear-index)))]
                     (recur (inc i)
                            (conj bin-index new-bin-index)
                            (conj linear-index new-linear-index)))
                   [bin-index linear-index]))]
    {:n-seq n-seq, :preset preset, :sc sc, :bc bc, :ec ec, :meta meta,
     :skip skip, :seq seq ,:bin-index (first index), :linear-index (last index)}))

(defn read-index
  [f]
  (with-open [r (DataInputStream. (BGZFInputStream. (cio/file f)))]
    (read-index* r)))

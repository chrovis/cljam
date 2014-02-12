(ns cljam.tabix
  (:require [clojure.java.io :as io]
            [cljam.lsb :as lsb])
  (:import java.util.Arrays
           [java.io DataInputStream IOException]
           bgzf4j.BGZFInputStream))

(set! *warn-on-reflection* true)

(def tabix-magic "TBI\1")

(def ^:private max-bin 37450)

(def ^:private tad-min-chunk-gap 32768)

(def ^:private tad-lidx-shift 14)

(defn- read-chunk
  [^DataInputStream rdr]
  [(lsb/read-long rdr) (lsb/read-long rdr)])

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
  (let [n-seq  (lsb/read-int rdr) _ (println "n-seq: " n-seq)
        preset (lsb/read-int rdr) _ (println "preset: " preset)
        sc     (lsb/read-int rdr) _ (println "sc: " sc)
        bc     (lsb/read-int rdr) _ (println "bc: " bc)
        ec     (lsb/read-int rdr) _ (println "ec: " ec)
        meta   (lsb/read-int rdr) _ (println "meta: " meta)
        skip   (lsb/read-int rdr) _ (println "skip: " skip)
        len    (lsb/read-int rdr) _ (println "len: " len)
        buf    (lsb/read-bytes rdr len)
        index  (loop [i 0
                      bin-index []
                      linear-index []]
                 (if (< i n-seq)
                   (let [n-bin (lsb/read-int rdr) _ (println "n-bin: " n-bin)
                         new-bin-index (doall (map (fn [_] (read-bin rdr)) (range n-bin)))
                         _ (println new-bin-index)
                         n-linear-index (lsb/read-int rdr) _ (println "n-linear-index: " n-linear-index)
                         new-linear-index (doall (map (fn [_] (lsb/read-long rdr)) (range n-linear-index)))]
                     (recur (inc i)
                            (conj bin-index new-bin-index)
                            (conj linear-index new-linear-index)))
                   [bin-index linear-index]))]
    {:n-seq n-seq, :preset preset, :sc sc, :bc bc, :ec ec, :meta meta,
     :skip skip, :bin-index (first index), :linear-index (last index)}
    ))

(deftype TABIXReader [f ^DataInputStream reader]
  java.io.Closeable
  (close [this]
    (.close ^DataInputStream (.reader this))))

(defn read-index
  [^TABIXReader rdr]
  (read-index* (.reader rdr)))

(defn reader [f]
  (->TABIXReader f (DataInputStream. (BGZFInputStream. (io/file f)))))

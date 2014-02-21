(ns cljam.bam-indexer.reader
  (:require [clojure.java.io :as io]
            [cljam.lsb :as lsb]
            [cljam.bam-indexer.common :refer [bai-magic]])
  (:import java.util.Arrays
           [java.io DataInputStream FileInputStream Closeable IOException]))

(deftype BAIReader [f reader]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this))))

(defn- read-chunks!
  [^DataInputStream rdr len]
  (loop [i 0, chunks []]
    (if (< i len)
      (recur (inc i) (conj chunks {:beg (lsb/read-long rdr)
                                   :end (lsb/read-long rdr)}))
      chunks)))

(defn- read-bin-index!
  [^DataInputStream rdr len]
  (loop [i 0, index []]
    (if (< i len)
      (let [bin (lsb/read-int rdr)
            n-chunk (lsb/read-int rdr)
            chunks (read-chunks! rdr n-chunk)]
        (recur (inc i) (conj index {:bin bin, :chunks chunks})))
      index)))

(defn- read-linear-index!
  [^DataInputStream rdr len]
  (loop [i 0, index []]
    (if (< i len)
      (recur (inc i) (conj index (lsb/read-long rdr)))
      index)))

(defn- read-index*!
  [^DataInputStream rdr]
  (when-not (Arrays/equals ^bytes (lsb/read-bytes rdr 4) (.getBytes ^String bai-magic))
    (throw (IOException. "Invalid BAI file")))
  (let [n-ref (lsb/read-int rdr)]
    (loop [i 0, index []]
      (if (< i n-ref)
        (let [n-bin-index (lsb/read-int rdr)
              new-bin-index (read-bin-index! rdr n-bin-index)
              n-linear-index (lsb/read-int rdr)
              new-linear-index (read-linear-index! rdr n-linear-index)]
          (recur (inc i) (conj index {:bin-index new-bin-index
                                      :linear-index new-linear-index})))
        index))))

(defn read-index
  [^BAIReader rdr]
  (read-index*! (.reader rdr)))

(defn reader [f]
  (->BAIReader f (DataInputStream. (FileInputStream. (io/file f)))))

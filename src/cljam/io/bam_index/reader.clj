(ns cljam.io.bam-index.reader
  (:require [clojure.java.io :as cio]
            [cljam.io.util.lsb.io-stream :as lsb]
            [cljam.io.bam-index.common :refer [bai-magic]]
            [cljam.io.util.chunk :as chunk]
            [cljam.util :as util])
  (:import java.util.Arrays
           [java.io FileInputStream Closeable IOException]
           [cljam.io.util.chunk Chunk]))

(deftype BAIReader [url reader]
  Closeable
  (close [this]
    (.close ^Closeable (.reader this))))

(defn- skip-chunks!
  [rdr]
  (let [n-chunks (int (lsb/read-int rdr))]
    (loop [i 0]
      (when (< i n-chunks)
        (lsb/skip rdr 16)
        (recur (inc i))))))

(defn- skip-bin-index!
  [rdr]
  (let [n-bidx (int (lsb/read-int rdr))]
    (loop [i 0]
      (when (< i n-bidx)
        (lsb/skip rdr 4)
        (skip-chunks! rdr)
        (recur (inc i))))))

(defn- skip-linear-index!
  [rdr]
  (let [n-lidx (int (lsb/read-int rdr))]
    (loop [i 0]
      (when (< i n-lidx)
        (lsb/skip rdr 8)
        (recur (inc i))))))

(defn- skip-index!
  [rdr ^long n]
  (loop [i 0]
    (when (< i n)
      (skip-bin-index! rdr)
      (skip-linear-index! rdr)
      (recur (inc i)))))

(defn- read-chunks!
  [rdr]
  (let [n (int (lsb/read-int rdr))]
    (loop [i 0, chunks []]
      (if (< i n)
        (recur (inc i) (conj chunks (Chunk. (lsb/read-long rdr) (lsb/read-long rdr))))
        chunks))))

(defn- read-bin-index**!
  [rdr]
  (let [n (int (lsb/read-int rdr))]
    (loop [i 0, bidx []]
      (if (< i n)
        (let [bin (lsb/read-int rdr)
              chunks (read-chunks! rdr)]
          (recur (inc i) (conj bidx {:bin bin, :chunks chunks})))
        bidx))))

(defn- read-bin-index*!
  [rdr ^long ref-idx]
  (let [n-ref (int (lsb/read-int rdr))]
    (when (>= ref-idx n-ref)
      (throw (IndexOutOfBoundsException. "The reference index number is invalid")))
    (skip-index! rdr ref-idx)
    (read-bin-index**! rdr)))

(defn- read-linear-index**!
  [rdr]
  (let [n (int (lsb/read-int rdr))]
    (loop [i 0, lidx []]
      (if (< i n)
        (recur (inc i) (conj lidx (lsb/read-long rdr)))
        lidx))))

(defn- read-linear-index*!
  [rdr ^long ref-idx]
  (let [n-ref (int (lsb/read-int rdr))]
    (when (>= ref-idx n-ref)
      (throw (IndexOutOfBoundsException. "The reference index number is invalid")))
    (skip-index! rdr ref-idx)
    (skip-bin-index! rdr)
    (read-linear-index**! rdr)))

(defn read-bin-index!
  "Reads a binning index eagerly for the given reference index.
  Returns a vector of maps containing :bin and :chunks."
  [^BAIReader rdr ref-idx]
  (read-bin-index*! (.reader rdr) ref-idx))

(defn read-linear-index!
  "Reads a linear index eagerly for the given reference index.
  Returns a vector of linear index numbers"
  [^BAIReader rdr ref-idx]
  (read-linear-index*! (.reader rdr) ref-idx))

(defn read-all-index!
  "Reads all linear indices and bin indices.
  Returns a vector of maps containing :bidx and :lidx."
  [^BAIReader r]
  (let [rdr (.reader r)
        n-ref (lsb/read-int rdr)
        refs (range n-ref)
        all-idx (map (fn [_] [(read-bin-index**! rdr) (read-linear-index**! rdr)]) refs)
        bidx-seq (map first all-idx)
        bidx (zipmap
              refs
              (map (fn [bins]
                     (zipmap (map :bin bins) (map :chunks bins)))
                   bidx-seq))
        lidx (zipmap refs (map second all-idx))]
    {:bidx bidx
     :lidx lidx}))

(defn reader
  "Returns an open cljam.io.bam_index.reader.BAIReader of f.
  Should be used inside with-open to ensure the reader is properly closed."
  [f]
  (let [url (util/as-url f)
        r (if (= (.getProtocol url) "file")
            (FileInputStream. (cio/file url))
            (.openStream url))]
    (when-not (Arrays/equals ^bytes (lsb/read-bytes r 4) (.getBytes ^String bai-magic))
      (throw (IOException. "Invalid BAI file")))
    (->BAIReader url r)))

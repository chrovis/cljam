(ns cljam.io.tabix
  "Alpha - subject to change.
  Reader of a TABIX format file."
  (:require [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.util.bin :as util-bin])
  (:import java.util.Arrays
           [java.io DataInputStream IOException]
           [cljam.io.bam_index.chunk Chunk]))

(deftype Tabix [n-ref preset sc bc ec meta skip seq bidx lidx]
  util-bin/IBinaryIndex
  (bidx-ref [this]
    (.bidx this))
  (lidx-ref [this]
    (.lidx this)))

(def tabix-magic "TBI\1")

(defn- read-chunks!
  [rdr]
  (let [n (lsb/read-int rdr)]
    (loop [i 0, chunks []]
      (if (< i n)
        (recur (inc i) (conj chunks (Chunk. (lsb/read-long rdr) (lsb/read-long rdr))))
        chunks))))

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

(defn- read-bin-index**!
  [rdr]
  (let [n (lsb/read-int rdr)]
    (loop [i 0, bidx []]
      (if (< i n)
        (let [bin (lsb/read-int rdr)
              chunks (read-chunks! rdr)]
          (recur (inc i) (conj bidx {:bin bin, :chunks chunks})))
        bidx))))

(defn- read-linear-index**!
  [rdr]
  (let [n (lsb/read-int rdr)]
    (loop [i 0, lidx []]
      (if (< i n)
        (recur (inc i) (conj lidx (lsb/read-long rdr)))
        lidx))))

(defn- read-index*
  [^DataInputStream rdr]
  (when-not (Arrays/equals ^bytes (lsb/read-bytes rdr 4) (.getBytes ^String tabix-magic))
    (throw (IOException. "Invalid TABIX file")))
  (let [n-ref  (lsb/read-int rdr)
        preset (lsb/read-int rdr)
        sc     (lsb/read-int rdr)
        bc     (lsb/read-int rdr)
        ec     (lsb/read-int rdr)
        meta   (lsb/read-int rdr)
        skip   (lsb/read-int rdr)
        len    (lsb/read-int rdr)
        buf    (lsb/read-bytes rdr len)
        seq    (read-seq buf len)
        refs (range n-ref)
        all-idx (map (fn [_] [(read-bin-index**! rdr) (read-linear-index**! rdr)]) refs)
        bidx-seq (map first all-idx)
        bidx (zipmap
              refs
              (map (fn [bins]
                     (zipmap (map :bin bins) (map :chunks bins)))
                   bidx-seq))
        lidx (zipmap refs (map second all-idx))]
    (->Tabix n-ref preset sc bc ec  meta
             skip  seq bidx lidx)))

(defn read-index
  [f]
  (with-open [r (DataInputStream. (bgzf/bgzf-input-stream f))]
    (read-index* r)))

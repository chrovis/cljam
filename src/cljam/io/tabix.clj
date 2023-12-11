(ns cljam.io.tabix
  "Alpha - subject to change.
  Reader of a TABIX format file."
  (:require [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.lsb.data-io :as lsb]
            [cljam.io.util.bin :as util-bin]
            [clojure.string :as cstr])
  (:import java.util.Arrays
           [java.io DataInputStream IOException]
           [cljam.io.util.chunk Chunk])
  (:refer-clojure :exclude [meta seq]))

(def ^:private ^:const linear-index-shift 14)
(def ^:private ^:const linear-index-depth 5)

(deftype Tabix [n-ref preset sc bc ec meta skip seq bidx lidx]
  util-bin/IBinningIndex
  (get-chunks [_ ref-idx bins]
    (vec (mapcat (get bidx ref-idx) bins)))
  (get-min-offset [_ ref-idx beg]
    (get (get lidx ref-idx)
         (util-bin/pos->lidx-offset beg linear-index-shift) 0))
  (get-min-shift [_] linear-index-shift)
  (get-depth [_]
    linear-index-depth)
  (get-chr-names [_]
    seq))

(def ^:private tabix-magic "TBI\1")

(defn- read-chunks!
  [rdr]
  (->> #(Chunk. (lsb/read-long rdr) (lsb/read-long rdr))
       (repeatedly (lsb/read-int rdr))
       vec))

(defn- read-seq
  [^bytes buf]
  (cstr/split (String. buf) #"\00"))

(defn- read-bin-index
  [rdr]
  (->> #(hash-map
         :bin (lsb/read-int rdr)
         :chunks (read-chunks! rdr))
       (repeatedly (lsb/read-int rdr))
       vec))

(defn- read-linear-index
  [rdr]
  (->> #(lsb/read-long rdr)
       (repeatedly (lsb/read-int rdr))
       vec))

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
        seq    (read-seq buf)
        refs (range n-ref)
        all-idx (map (fn [_] [(read-bin-index rdr) (read-linear-index rdr)]) refs)
        bidx-seq (map first all-idx)
        bidx (zipmap
              refs
              (map (fn [bins]
                     (into {} (map (juxt :bin :chunks)) bins))
                   bidx-seq))
        lidx (zipmap refs (map second all-idx))]
    (->Tabix n-ref preset sc bc ec  meta
             skip  seq bidx lidx)))

(defn read-index
  "Reads tabix and returns Tabix object."
  [f]
  (with-open [r (DataInputStream. (bgzf/bgzf-input-stream f))]
    (read-index* r)))

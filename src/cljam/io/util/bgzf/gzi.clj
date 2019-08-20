(ns cljam.io.util.bgzf.gzi
  (:require [cljam.util :as util])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.nio.channels FileChannel FileChannel$MapMode]
           [java.nio.file Paths OpenOption StandardOpenOption]))

(defn- read-full ^ByteBuffer [f]
  (with-open [c (FileChannel/open
                 (Paths/get (.toURI (util/as-url f)))
                 (into-array OpenOption [StandardOpenOption/READ]))]
    (.. c
        (map FileChannel$MapMode/READ_ONLY 0 (.size c))
        (order ByteOrder/LITTLE_ENDIAN))))

(defn- parse-gzi [^ByteBuffer bb]
  (let [n-entries (.getLong bb)]
    (->> #(let [compressed (.getLong bb)
                uncompressed (.getLong bb)]
            [uncompressed compressed])
         (repeatedly n-entries)
         (into (sorted-map 0 0)))))

(defn read-gzi
  "Reads a .gzi file and returns a sorted-map of uncompressed offsets to
  compressed offsets."
  [f]
  (parse-gzi (read-full f)))

(defn uncomp->comp
  "Returns a virtual file offset for a given uncompressed offset."
  ^long [gzi ^long uncompressed-offset]
  (let [[u c] (first (rsubseq gzi <= uncompressed-offset))
        diff (- uncompressed-offset (long u))]
    (assert (<= 0 diff 0xffff))
    (bit-or (bit-shift-left (long c) 16)
            (bit-and diff 0xffff))))

(defn comp->uncomp
  "Returns an uncompressed offset for a given virtual file offset"
  ^long [gzi ^long compressed-offset]
  (let [off (unsigned-bit-shift-right compressed-offset 16)
        [uncompressed] (->> gzi
                            rseq
                            (filter (fn [[_ c]] (= (long c) off)))
                            first)]
    (assert uncompressed)
    (+ uncompressed (bit-and compressed-offset 0xffff))))

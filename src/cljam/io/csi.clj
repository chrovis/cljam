(ns cljam.io.csi
  "Reader of a CSI format file."
  (:require [cljam.io.util.bgzf :as bgzf]
            [cljam.io.util.lsb :as lsb]
            [cljam.io.util.chunk :as chunk]
            [cljam.io.util.bin :as util-bin])
  (:import java.util.Arrays
           [java.io DataInputStream IOException]))

(deftype CSI [n-ref min-shift depth bidx loffset]
  util-bin/IBinningIndex
  (get-chunks [_ ref-idx bins]
    (vec (mapcat (get bidx ref-idx) bins)))
  (get-min-offset [_ ref-idx beg]
    (if-let [min-offsets (rsubseq (get loffset ref-idx) <= beg)]
      (second (first min-offsets))
      0))
  (get-depth [_]
    depth)
  (get-min-shift [_]
    min-shift))

(def ^:const ^:private csi-magic "CSI\1")

(defn- read-chunks!
  [rdr]
  (let [n-chunk (lsb/read-int rdr)]
    (->> #(let [beg (lsb/read-long rdr) end (lsb/read-long rdr)]
            (chunk/->Chunk beg end))
         (repeatedly n-chunk)
         vec)))

(defn- read-bin-index
  [rdr]
  (let [n-ref (lsb/read-int rdr)]
    (->> #(let [bin (lsb/read-int rdr)
                loffset (lsb/read-long rdr)
                chunks (read-chunks! rdr)]
            (hash-map
             :bin bin
             :loffset loffset
             :chunks chunks))
         (repeatedly n-ref)
         vec)))

(defn- read-index*
  [^DataInputStream rdr]
  (when-not (Arrays/equals ^bytes (lsb/read-bytes rdr 4) (.getBytes csi-magic))
    (throw (IOException. "Invalid CSI file")))
  (let [min-shift (lsb/read-int rdr)
        depth (lsb/read-int rdr)
        l-aux (lsb/read-int rdr)
        _ (lsb/read-bytes rdr l-aux)
        n-ref (lsb/read-int rdr)
        refs (range n-ref)
        bins (vec (repeatedly n-ref #(read-bin-index rdr)))
        max-bin (util-bin/max-bin depth)
        bidx (zipmap refs
                     (map (fn [bin]
                            (into {} (comp (map (juxt :bin :chunks))
                                           (filter #(<= (first %) max-bin)))
                                  bin))
                          bins))
        loffset (zipmap refs
                        (map (fn [bin]
                               (into (sorted-map)
                                     (comp (map (juxt :bin :loffset))
                                           (filter #(<= (first %) max-bin))
                                           (map (fn [[bin loffset]]
                                                  [(util-bin/bin-beg bin
                                                                     min-shift
                                                                     depth)
                                                   loffset])))
                                     bin))
                             bins))]
    (->CSI n-ref min-shift depth bidx loffset)))

(defn read-index
  [f]
  (with-open [r (DataInputStream. (bgzf/bgzf-input-stream f))]
    (read-index* r)))

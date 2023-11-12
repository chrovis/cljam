(ns cljam.io.twobit.writer
  (:require [cljam.io.protocols :as protocols]
            [cljam.io.util.byte-buffer :as bb]
            [cljam.util :as util]
            [clojure.java.io :as cio])
  (:import [java.io Closeable]
           [java.nio Buffer ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.file OpenOption StandardOpenOption]))

(declare flush-buffer! write-sequences)

(deftype TwoBitWriter [url channel buffer index]
  Closeable
  (close [this]
    (flush-buffer! this)
    (.close ^Closeable (.-channel this)))
  protocols/IWriter
  (writer-url [this]
    (.-url this))
  protocols/ISequenceWriter
  (write-sequences [this seqs]
    (write-sequences this seqs)))

(defn writer
  "Returns a 2bit writer of f."
  [f {:keys [index]}]
  (let [file (cio/file f)
        ch (FileChannel/open (.toPath file)
                             (into-array OpenOption [StandardOpenOption/WRITE
                                                     StandardOpenOption/CREATE
                                                     StandardOpenOption/TRUNCATE_EXISTING]))
        bb (bb/allocate-lsb-byte-buffer 8192)]
    (TwoBitWriter. (util/as-url (.getAbsolutePath file)) ch bb (some-> index vec))))

(defn- flush-buffer! [^TwoBitWriter w]
  (let [^FileChannel ch (.-channel w)
        ^Buffer bb (.-buffer w)]
    (.flip bb)
    (while (.hasRemaining bb)
      (.write ch ^ByteBuffer bb))
    (.clear bb)))

(defn- ensure-buffer-room! [^TwoBitWriter w ^long n]
  (when (< (.remaining ^ByteBuffer (.-buffer w)) n)
    (flush-buffer! w)))

(defn- write-file-header!
  "Writes a 2bit file header. Supports little-endian only."
  [^TwoBitWriter w nseq]
  (ensure-buffer-room! w 16)
  (doto ^ByteBuffer (.-buffer w)
    (.putInt 0x1A412743)
    (.putInt 0)
    (.putInt nseq)
    (.putInt 0)))

(defn- mask-regions
  "Returns a sequence of [start length] of masked regions."
  [^String s]
  (let [len (.length s)]
    (loop [r (transient [])
           p nil
           l nil
           i 0]
      (if (= i len)
        (if p
          (persistent! (conj! r [p l]))
          (persistent! r))
        (if (<= (int \a) (int (.charAt s (int i))))
          (if p
            (recur r p (inc (long l)) (inc i))
            (recur r i 1 (inc i)))
          (if p
            (recur (conj! r [p l]) nil nil (inc i))
            (recur r nil nil (inc i))))))))

(defn- amb-regions
  "Returns a sequence of [start length] of N regions."
  [^String s]
  (let [len (.length s)]
    (loop [r (transient [])
           p nil
           l nil
           i 0]
      (if (= i len)
        (if p
          (persistent! (conj! r [p l]))
          (persistent! r))
        (if (= \N (.charAt s (int i)))
          (if p
            (recur r p (inc (long l)) (inc i))
            (recur r i 1 (inc i)))
          (if p
            (recur (conj! r [p l]) nil nil (inc i))
            (recur r nil nil (inc i))))))))

(defn- write-index!
  [^TwoBitWriter w idx]
  (let [^ByteBuffer bb (.-buffer w)]
    (loop [offset (+ (* 4 4)
                     (long (reduce + (map #(+ 1 (count (:name %)) 4) idx))))
           idx idx]
      (when-let [{:keys [len ambs masks] name' :name} (first idx)]
        (let [name-size (count name')]
          (ensure-buffer-room! w (+ name-size 5))
          (doto bb
            (bb/write-ubyte (count name'))
            (bb/write-string name')
            (.putInt offset))
          (recur (+ offset
                    (if (and ambs masks)
                      (+ 4 4 (* 2 4 (count ambs)) 4 (* 2 4 (count masks)) 4)
                      0) ; dummy
                    (quot (dec (+ (long len) 4)) 4))
                 (next idx)))))))

(def ^:private
  char->twobit
  (doto (byte-array 128)
    (aset (int \C) (byte 1))
    (aset (int \c) (byte 1))
    (aset (int \A) (byte 2))
    (aset (int \a) (byte 2))
    (aset (int \G) (byte 3))
    (aset (int \g) (byte 3))))

(defn write-twobit!
  "Encodes a sequence into twobit format."
  [^TwoBitWriter w ^String s]
  (let [len (.length s)
        in (ByteBuffer/wrap (.getBytes s))
        out ^ByteBuffer (.-buffer w)
        table ^bytes char->twobit
        encode-four-bases #(->> (bit-or
                                 (bit-shift-left (aget table (.get in)) 6)
                                 (bit-shift-left (aget table (.get in)) 4)
                                 (bit-shift-left (aget table (.get in)) 2)
                                 (aget table (.get in)))
                                unchecked-byte
                                (.put out))]
    ;; write out per chunk of size smaller than buffer capacity
    (dotimes [_ (quot len 1024)]
      (ensure-buffer-room! w 256)
      (dotimes [_ 256]
        (encode-four-bases)))
    (let [remaining (rem len 1024)]
      (when (pos? remaining)
        (ensure-buffer-room! w (quot (+ remaining 3) 4))
        (dotimes [_ (quot remaining 4)]
          (encode-four-bases))
        (when (pos? (rem remaining 4))
          (loop [b 0 i (rem remaining 4) j 1]
            (if (pos? i)
              (recur
               (bit-or b (bit-shift-left (aget table (.get in)) (* 2 (- 4 j))))
               (dec i)
               (inc j))
              (.put out (unchecked-byte b)))))))))

(defn- write-sequence!
  "Writes a single sequence entry to writer."
  [^TwoBitWriter w sequence' idx]
  (let [name' (or (:name sequence') (:rname sequence'))
        seq-data (or (:seq sequence') (:sequence sequence'))
        {:keys [len ambs masks]} (first (filter #(= (:name %) name') idx))
        ^ByteBuffer bb (.-buffer w)
        write-int (fn [n]
                    (ensure-buffer-room! w 4)
                    (.putInt bb n))]
    (write-int len)
    (write-int (count ambs))
    (doseq [[s _] ambs]
      (write-int s))
    (doseq [[_ l] ambs]
      (write-int l))
    (write-int (count masks))
    (doseq [[s _] masks]
      (write-int s))
    (doseq [[_ l] masks]
      (write-int l))
    (write-int 0)
    (write-twobit! w seq-data)))

(defn- write-sequences-without-index
  [wtr xs]
  (let [idx (map (fn [{:keys [rname]
                       seq' :seq
                       sequence' :sequence
                       name' :name}]
                   (let [seq-data (or seq' sequence')]
                     {:name (or name' rname)
                      :len (count seq-data)
                      :masks (mask-regions seq-data)
                      :ambs (amb-regions seq-data)}))
                 xs)]
    (write-file-header! wtr (count xs))
    (write-index! wtr idx)
    (doseq [sequence' xs]
      (write-sequence! wtr sequence' idx))))

(defn- write-sequences-with-index
  [^TwoBitWriter wtr idx xs]
  (let [idx-atom (atom idx)]
    (write-file-header! wtr (count @idx-atom))
    (write-index! wtr @idx-atom)
    (doseq [sequence' xs]
      (let [name' (or (:name sequence') (:rname sequence'))
            seq-data (or (:seq sequence') (:sequence sequence'))
            masks (mask-regions seq-data)
            ambs (amb-regions seq-data)
            i (first (keep-indexed #(when (= (:name %2) name') %1) @idx-atom))]
        (swap! idx-atom update i assoc :masks masks :ambs ambs))
      (write-sequence! wtr sequence' @idx-atom))
    ;; finalize
    (flush-buffer! wtr)
    (.position ^FileChannel (.-channel wtr) 16)
    (write-index! wtr @idx-atom)))

(defn write-sequences
  "Writes all sequences to wtr. Input sequences must be a sequence of maps."
  [^TwoBitWriter wtr xs]
  (if (nil? (.index wtr))
    (write-sequences-without-index wtr xs)
    (write-sequences-with-index wtr (.index wtr) xs)))

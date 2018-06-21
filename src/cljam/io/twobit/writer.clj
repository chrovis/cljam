(ns cljam.io.twobit.writer
  (:require [clojure.java.io :as cio]
            [cljam.io.protocols :as protocols]
            [cljam.io.util.lsb :as lsb]
            [cljam.util :as util])
  (:import [java.io Closeable OutputStream DataOutputStream BufferedOutputStream FileOutputStream]
           [java.nio ByteBuffer]))

(declare write-sequences)

(deftype TwoBitWriter [url writer file-output-stream index]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this)))
  protocols/IWriter
  (writer-url [this]
    (.url this))
  protocols/ISequenceWriter
  (write-sequences [this seqs]
    (write-sequences this seqs)))

(defn writer
  "Returns a 2bit writer of f."
  [f {:keys [index]}]
  (let [abs-f (.getAbsolutePath (cio/file f))
        fos (FileOutputStream. abs-f)
        bos (BufferedOutputStream. fos)
        dos (DataOutputStream. bos)]
    (TwoBitWriter. (util/as-url abs-f) dos fos index)))

(defn- write-file-header!
  "Writes a 2bit file header. Supports little-endian only."
  [w nseq]
  (lsb/write-int w 0x1A412743)
  (lsb/write-int w 0)
  (lsb/write-int w nseq)
  (lsb/write-int w 0))

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
            (recur r p (inc l) (inc i))
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
            (recur r p (inc l) (inc i))
            (recur r i 1 (inc i)))
          (if p
            (recur (conj! r [p l]) nil nil (inc i))
            (recur r nil nil (inc i))))))))

(defn- index-size
  "Number of bytes required for index."
  [seqs]
  (-> (fn [{:keys [name]}]
        (+ 1 (count name) 4))
      map
      (transduce + 0 seqs)))

(defn- write-index!
  [w idx]
  (loop [offset (+ (* 4 4) (reduce + (map #(+ 1 (count (:name %)) 4) idx)))
         idx idx]
    (when-let [{:keys [name len]} (first idx)]
      (lsb/write-ubyte w (count name))
      (lsb/write-string w name)
      (lsb/write-int w offset)
      (recur (+ offset
                (if-let [{:keys [ambs masks]} (first idx)]
                  (+ 4 4 (* 2 4 (count ambs)) 4 (* 2 4 (count masks)) 4)
                  0) ; dummy
                (quot (dec (+ len 4)) 4))
             (next idx)))))

(def ^:private
  char->twobit
  (doto (byte-array 128)
    (aset-byte (int \C) 1)
    (aset-byte (int \c) 1)
    (aset-byte (int \A) 2)
    (aset-byte (int \a) 2)
    (aset-byte (int \G) 3)
    (aset-byte (int \g) 3)))

(defn write-twobit!
  "Encodes a sequence into twobit format."
  [^OutputStream o ^String s]
  (let [len (.length s)
        bb (ByteBuffer/wrap (.getBytes s))
        table ^bytes char->twobit]
    (dotimes [_ (quot len 4)]
      (->> (bit-or
            (bit-shift-left (aget table (.get bb)) 6)
            (bit-shift-left (aget table (.get bb)) 4)
            (bit-shift-left (aget table (.get bb)) 2)
            (aget table (.get bb)))
           unchecked-int
           (.write o)))
    (when (pos? (mod len 4))
      (loop [b 0 i (mod len 4) j 1]
        (if (pos? i)
          (recur (bit-or b (bit-shift-left (aget table (.get bb)) (* 2 (- 4 j)))) (dec i) (inc j))
          (.write o (unchecked-int b)))))))

(defn- write-sequence!
  "Writes a single sequence entry to writer."
  [w sequence idx]
  (let [name (or (:name sequence) (:rname sequence))
        seq-data (or (:seq sequence) (:sequence sequence))
        {:keys [len ambs masks]} (first (filter #(= (:name %) name) idx))]
    (lsb/write-int w len)
    (lsb/write-int w (count ambs))
    (doseq [[s _] ambs]
      (lsb/write-int w s))
    (doseq [[_ l] ambs]
      (lsb/write-int w l))
    (lsb/write-int w (count masks))
    (doseq [[s _] masks]
      (lsb/write-int w s))
    (doseq [[_ l] masks]
      (lsb/write-int w l))
    (lsb/write-int w 0)
    (write-twobit! w seq-data)))

(defn- write-sequences-without-index
  [^TwoBitWriter wtr xs]
  (let [idx (map (fn [{:keys [name rname seq sequence]}]
                   (let [seq-data (or seq sequence)]
                     {:name (or name rname)
                      :len (count seq-data)
                      :masks (mask-regions seq-data)
                      :ambs (amb-regions seq-data)}))
                 xs)]
    (write-file-header! (.writer wtr) (count xs))
    (write-index! (.writer wtr) idx)
    (doseq [sequence xs]
      (write-sequence! (.writer wtr) sequence idx))))

(defn- write-sequences-with-index
  [^TwoBitWriter wtr idx xs]
  (let [idx-atom (atom idx)]
    (write-file-header! (.writer wtr) (count @idx-atom))
    (write-index! (.writer wtr) @idx-atom)
    (doseq [sequence xs]
      (let [name (or (:name sequence) (:rname sequence))
            seq-data (or (:seq sequence) (:sequence sequence))
            masks (mask-regions seq-data)
            ambs (amb-regions seq-data)
            i (first (keep-indexed #(if (= (:name %2) name) %1) @idx-atom))]
        (swap! idx-atom update i assoc :masks masks :ambs ambs))
      (write-sequence! (.writer wtr) sequence @idx-atom))
    ;; finalize
    (.flush ^DataOutputStream (.writer wtr))
    (let [ch (.getChannel ^FileOutputStream (.file-output-stream wtr))]
      (.position ch 16)
      (write-index! ch @idx-atom))))

(defn write-sequences
  "Writes all sequences to wtr. Input sequences must be a sequence of maps."
  [^TwoBitWriter wtr xs]
  (if (nil? (.index wtr))
    (write-sequences-without-index wtr xs)
    (write-sequences-with-index wtr (.index wtr) xs)))

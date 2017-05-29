(ns cljam.twobit.writer
  (:require [clojure.java.io :as cio]
            [cljam.io :as io]
            [cljam.lsb :as lsb])
  (:import [java.io Closeable OutputStream DataOutputStream BufferedOutputStream FileOutputStream]
           [java.nio ByteBuffer]))

(declare write-sequences)

(deftype TwoBitWriter [f writer]
  Closeable
  (close [this]
    (.close ^Closeable (.writer this)))
  io/IWriter
  (writer-path [this]
    (.f this))
  io/ISequenceWriter
  (write-sequences [this seqs]
    (write-sequences this seqs)))

(defn writer
  "Returns a 2bit writer of f."
  [f]
  (let [abs-f (.getAbsolutePath (cio/file f))
        fos (FileOutputStream. abs-f)
        bos (BufferedOutputStream. fos)
        dos (DataOutputStream. bos)]
    (TwoBitWriter. abs-f dos)))

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
  "Writes index section to writer."
  [w seqs]
  (loop [offset (+ (* 4 4) (index-size seqs)) xs seqs]
    (when-let [{:keys [name sequence masks ambs]} (first xs)]
      (let [header-size (+ 4 4 (* 2 4 (count ambs)) 4 (* 2 4 (count masks)) 4)
            seq-size (quot (dec (+ (count sequence) 4)) 4)]
        (lsb/write-ubyte w (count name))
        (lsb/write-string w name)
        (lsb/write-int w offset)
        (recur (+ offset header-size seq-size) (next xs))))))

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
      (loop [b 0 i (mod len 4)]
        (if (pos? i)
          (recur (bit-or b (bit-shift-left (aget table (.get bb)) (* 2 (- 4 i)))) (dec i))
          (.write o (unchecked-int b)))))))

(defn- write-sequence!
  "Writes a single sequence entry to writer."
  [w {:keys [sequence masks ambs]}]
  (let []
    (lsb/write-int w (count sequence))
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
    (write-twobit! w sequence)))

(defn write-sequences
  "Write all sequences to wtr.
  Input sequences must be a sequence of maps."
  [^TwoBitWriter wtr xs]
  (let [seqs (map (fn [{:keys [name rname seq sequence]}]
                    (let [chr-name (or name rname)
                          seq-data (or seq sequence)]
                      {:name chr-name
                       :sequence seq-data
                       :masks (mask-regions seq-data)
                       :ambs (amb-regions seq-data)}))
                  xs)]
    (write-file-header! (.writer wtr) (count xs))
    (write-index! (.writer wtr) seqs)
    (doseq [s seqs]
      (write-sequence! (.writer wtr) s))))

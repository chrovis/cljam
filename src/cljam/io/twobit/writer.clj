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

(defmacro ^:private long-pair [x y]
  `(doto (long-array 2)
     (aset 0 ~x)
     (aset 1 ~y)))

(defn- masked-regions [^bytes bs]
  (let [len (alength bs)]
    (loop [i 0
           masks (transient [])
           mask-start -1
           ambs (transient [])
           amb-start -1]
      (let [mask? (>= mask-start 0)
            amb? (>= amb-start 0)]
        (if (= i len)
          [(persistent!
            (cond-> masks mask? (conj! (long-pair mask-start (- i mask-start)))))
           (persistent!
            (cond-> ambs amb? (conj! (long-pair amb-start (- i amb-start)))))]
          (let [b (int (aget bs i))]
            (if (< b (int \a))
              (let [masks' (cond-> masks mask? (conj! (long-pair mask-start (- i mask-start))))]
                (if (= b (int \N))
                  (let [amb-start' (if amb? amb-start i)]
                    (recur (inc i) masks' -1 ambs amb-start'))
                  (let [ambs' (cond-> ambs amb? (conj! (long-pair amb-start (- i amb-start))))]
                    (recur (inc i) masks' -1 ambs' -1))))
              (let [mask-start' (if mask? mask-start i)]
                (if (= b (int \n))
                  (let [amb-start' (if amb? amb-start i)]
                    (recur (inc i) masks mask-start' ambs amb-start'))
                  (let [ambs' (cond-> ambs amb? (conj! (long-pair amb-start (- i amb-start))))]
                    (recur (inc i) masks mask-start' ambs' -1)))))))))))

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

(defn- write-twobit-bytes! [^TwoBitWriter w ^bytes bs]
  (let [len (count bs)
        in (ByteBuffer/wrap bs)
        out ^ByteBuffer (.-buffer w)
        table ^bytes char->twobit
        encode-four-bases #(->> (bit-or
                                 (bit-shift-left (long (aget table (.get in))) 6)
                                 (bit-shift-left (long (aget table (.get in))) 4)
                                 (bit-shift-left (long (aget table (.get in))) 2)
                                 (long (aget table (.get in))))
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
               (bit-or b (bit-shift-left (long (aget table (.get in))) (* 2 (- 4 j))))
               (dec i)
               (inc j))
              (.put out (unchecked-byte b)))))))))

(defn write-twobit!
  "Encodes a sequence into twobit format."
  [w ^String s]
  (write-twobit-bytes! w (.getBytes s)))

(defn- write-sequence!
  "Writes a single sequence entry to writer."
  [^TwoBitWriter w {:keys [len ambs masks]} bs]
  (let [^ByteBuffer bb (.-buffer w)
        write-int (fn [n]
                    (ensure-buffer-room! w 4)
                    (.putInt bb n))]
    (write-int len)
    (write-int (count ambs))
    (run! #(write-int (aget ^longs % 0)) ambs)
    (run! #(write-int (aget ^longs % 1)) ambs)
    (write-int (count masks))
    (run! #(write-int (aget ^longs % 0)) masks)
    (run! #(write-int (aget ^longs % 1)) masks)
    (write-int 0)
    (write-twobit-bytes! w bs)))

(defn- write-sequences-without-index
  [wtr xs]
  (let [idx (map (fn [x]
                   (let [^String cs (or (:seq x) (:sequence x))
                         bs (some-> cs (.getBytes))
                         [masks ambs] (masked-regions bs)]
                     {:name (or (:name x) (:rname x))
                      :seq bs
                      :len (count bs)
                      :masks masks
                      :ambs ambs}))
                 xs)]
    (write-file-header! wtr (count xs))
    (write-index! wtr idx)
    (doseq [entry idx]
      (write-sequence! wtr entry (:seq entry)))))

(defn- write-sequences-with-index
  [^TwoBitWriter wtr idx xs]
  (let [idx-atom (volatile! idx)]
    (write-file-header! wtr (count @idx-atom))
    (write-index! wtr @idx-atom)
    (doseq [x xs]
      (let [name' (or (:name x) (:rname x))
            ^String cs (or (:seq x) (:sequence x))
            bs (some-> cs (.getBytes))
            [masks ambs] (masked-regions bs)
            i (first (keep-indexed #(when (= (:name %2) name') %1) @idx-atom))]
        (vswap! idx-atom update i assoc :masks masks :ambs ambs)
        (write-sequence! wtr (nth @idx-atom i) bs)))
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

(ns cljam.lsb
  (:refer-clojure :exclude [read-string])
  (:require [cljam.util :refer [string->bytes bytes->string]])
  (:import [java.io DataInputStream DataOutputStream EOFException]
           [java.nio ByteBuffer ByteOrder]))

(defn ^ByteBuffer gen-byte-buffer
  ([]
     (.order (ByteBuffer/allocate 8) ByteOrder/LITTLE_ENDIAN))
  ([size]
     (.order (ByteBuffer/allocate size) ByteOrder/LITTLE_ENDIAN)))

(defmulti skip (fn [r & _] (class r)))

(defmethod skip DataInputStream
  [^DataInputStream rdr ^Integer n]
  (.skipBytes rdr n)
  nil)

(defmethod skip ByteBuffer
  [^ByteBuffer bb ^Integer n]
  (.position bb (+ (.position bb) n))
  nil)

;;; reading

(defmulti read-bytes (fn [r & _] (class r)))

(defmethod read-bytes DataInputStream
  ([^DataInputStream rdr l]
     (let [ba (byte-array l)]
       (.read rdr ba 0 l)
       ba))
  ([^DataInputStream rdr buffer offset l]
     (loop [total-read 0]
       (when (< total-read l)
         (let [n (.read rdr buffer (+ offset total-read) (- l total-read))]
           (if (neg? n)
             (throw (EOFException. "Premature EOF"))
             (recur (+ total-read n))))))))

(defmethod read-bytes ByteBuffer
  ([^ByteBuffer bb len]
     (let [ba (byte-array len)]
       (.get bb ba 0 len)
       ba))
  ([^ByteBuffer bb buffer offset len]
     (.get bb buffer offset len)))

(defn- read-byte-buffer
  [rdr ^ByteBuffer bb l]
  {:pre (< l (.capacity bb))}
  (read-bytes rdr (.array bb) 0 l)
  (.limit bb (.capacity bb))
  (.position bb l))

(defn read-ubyte
  [rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 1)
    (.put bb (byte 0))
    (.flip bb)
    (.getShort bb)))

(defn read-short
  [rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 2)
    (.flip bb)
    (.getShort bb)))

(defn read-ushort
  [rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 2)
    (.putShort bb (short 0))
    (.flip bb)
    (.getInt bb)))

(defn read-int
  [rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 4)
    (.flip bb)
    (.getInt bb)))

(defn read-long
  [rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 8)
    (.flip bb)
    (.getLong bb)))

(defn read-float
  [rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 4)
    (.flip bb)
    (.getFloat bb)))

(defn read-string
  [rdr ^Integer len]
  (String. ^bytes (read-bytes rdr len) 0 0 len))

(defn read-null-terminated-string
  [^ByteBuffer bb]
  (.mark bb)
  (let [start (.position bb)
        end (do (while (not (zero? (.get bb))))
                (.position bb))
        ba (byte-array (- end start 1))]
    (.reset bb)
    (.get bb ba)
    (.get bb)
    (bytes->string ba)))

;;; writing

(defn write-char
  [^DataOutputStream w b]
  (let [bb (gen-byte-buffer)]
    (.putChar bb b)
    (.write w (.array bb) 0 1)
    nil))

(defn write-bytes
  [^DataOutputStream w ^bytes b]
  (.write w b 0 (count b))
  nil)

(defn write-ubyte
  [^DataOutputStream w b]
  (let [bb (gen-byte-buffer)]
    (.putShort bb b)
    (.write w (.array bb) 0 1)
    nil))

(defn write-short
  [^DataOutputStream w n]
  (let [bb (gen-byte-buffer)]
    (.putShort bb n)
    (.write w (.array bb) 0 2)
    nil))

(defn write-ushort
  [^DataOutputStream w n]
  (let [bb (gen-byte-buffer)]
    (.putInt bb n)
    (.write w (.array bb) 0 2)
    nil))

(defn write-int
  [^DataOutputStream w n]
  (let [bb (gen-byte-buffer)]
    (.putInt bb n)
    (.write w (.array bb) 0 4)
    nil))

(defn write-long
  [^DataOutputStream w n]
  (let [bb (gen-byte-buffer)]
    (.putLong bb n)
    (.write w (.array bb) 0 8)
    nil))

(defn write-float
  [^DataOutputStream w n]
  (let [bb (gen-byte-buffer)]
    (.putFloat bb n)
    (.write w (.array bb) 0 4)
    nil))

(defn write-string
  [^DataOutputStream w s]
  (let [data-bytes (string->bytes s)]
   (.write w data-bytes 0 (count data-bytes))
   nil))

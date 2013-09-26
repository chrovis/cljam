(ns cljam.lsb
  (:refer-clojure :exclude [read-string])
  (:require [cljam.util :refer [string->bytes bytes->string]])
  (:import [java.io DataInputStream DataOutputStream EOFException]
           [java.nio ByteBuffer ByteOrder]))

(defn- ^ByteBuffer gen-byte-buffer []
  (.order (ByteBuffer/allocate 8) ByteOrder/LITTLE_ENDIAN))

(defn skip
  [^DataInputStream rdr ^Integer n]
  (.skipBytes rdr n)
  nil)

;;; reading

(defn read-bytes
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

(defn- read-byte-buffer
  [^DataInputStream rdr ^ByteBuffer bb l]
  {:pre (< l (.capacity bb))}
  (read-bytes rdr (.array bb) 0 l)
  (.limit bb (.capacity bb))
  (.position bb l))

(defn read-ubyte
  [^DataInputStream rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 1)
    (.put bb (byte 0))
    (.flip bb)
    (.getShort bb)))

(defn read-short
  [^DataInputStream rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 2)
    (.flip bb)
    (.getShort bb)))

(defn read-ushort
  [^DataInputStream rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 2)
    (.putShort bb (short 0))
    (.flip bb)
    (.getInt bb)))

(defn read-int
  [^DataInputStream rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 4)
    (.flip bb)
    (.getInt bb)))

(defn read-float
  [^DataInputStream rdr]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer rdr bb 4)
    (.flip bb)
    (.getFloat bb)))

(defn read-string
  [^DataInputStream rdr ^long l]
  (String. ^bytes (read-bytes rdr l) 0 0 l))

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

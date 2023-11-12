(ns cljam.io.util.lsb.data-io
  "Functions for reading little-endian data using DataInput."
  (:refer-clojure :exclude [read-string])
  (:require [cljam.io.util.byte-buffer :as bb])
  (:import [java.io ByteArrayOutputStream DataInput]))

(defn skip
  "Skips over 'length' bytes of data, discarding the skipped bytes."
  [^DataInput input ^long length]
  (.skipBytes input length))

(defn read-byte
  "Reads 1 byte. Returns a byte value."
  [^DataInput input]
  (.readByte input))

(defn read-ubyte
  "Reads 1 byte. Returns an unsigned byte value as long."
  [^DataInput input]
  (.readUnsignedByte input))

(defn read-short
  "Reads 2 bytes. Returns a short value."
  [^DataInput input]
  (let [bb (bb/allocate-lsb-byte-buffer 2)]
    (.readFully input (.array bb))
    (.getShort bb)))

(defn read-ushort
  "Reads 2 bytes. Returns an unsigned short value as long."
  [^DataInput input]
  (bit-and (short (read-short input)) 0xFFFF))

(defn read-int
  "Reads 4 bytes. Returns an int value."
  [^DataInput input]
  (let [bb (bb/allocate-lsb-byte-buffer 4)]
    (.readFully input (.array bb))
    (.getInt bb)))

(defn read-uint
  "Reads 4 bytes. Returns an unsigned int value as long."
  [^DataInput input]
  (bit-and (int (read-int input)) 0xFFFFFFFF))

(defn read-long
  "Reads 8 bytes. Returns a long value. "
  [^DataInput input]
  (let [bb (bb/allocate-lsb-byte-buffer 8)]
    (.readFully input (.array bb))
    (.getLong bb)))

(defn read-float
  "Reads 4 bytes. Returns a float value."
  [^DataInput input]
  (Float/intBitsToFloat (read-int input)))

(defn read-double
  "Reads 8 bytes. Returns a double value."
  [^DataInput input]
  (Double/longBitsToDouble (read-long input)))

(defn read-bytes
  "Reads 'length' bytes to buffer starting from offset bytes. Returns a new byte-array if called without buffer."
  ([^DataInput input ^long length]
   (let [ba (byte-array length)]
     (.readFully input ba)
     ba))
  ([^DataInput input buffer ^long offset ^long length]
   (.readFully input buffer offset length)
   buffer))

(defn read-string
  "Reads 'length' bytes. Returns a String."
  [^DataInput input ^long length]
  (let [ba (byte-array length)]
    (.readFully input ba)
    (String. ba)))

(defn read-null-terminated-string
  "Reads until next null character. Returns a String without the null."
  [^DataInput input]
  (with-open [baos (ByteArrayOutputStream. 32)]
    (loop []
      (let [b (.readByte input)]
        (when-not (zero? b)
          (.write baos b)
          (recur))))
    (.toString baos)))

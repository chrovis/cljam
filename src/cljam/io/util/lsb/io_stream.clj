(ns cljam.io.util.lsb.io-stream
  "Functions for reading/writing little-endian data using InputStream/OutputStream."
  (:refer-clojure :exclude [read-string])
  (:require [cljam.io.util.byte-buffer :as bb]
            [cljam.util :refer [string->bytes]])
  (:import [java.io
            ByteArrayOutputStream
            EOFException
            InputStream
            OutputStream]))

(declare read-bytes)

(defn skip
  "Skips over 'length' bytes of data, discarding the skipped bytes."
  [^InputStream stream ^long length]
  (.skip stream length))

(defn read-byte
  "Reads 1 byte. Returns a byte value."
  [^InputStream stream]
  (unchecked-byte (.read stream)))

(defn read-ubyte
  "Reads 1 byte. Returns an unsigned byte value as long."
  [^InputStream stream]
  (bit-and (.read stream) 0xFF))

(defn read-short
  "Reads 2 bytes. Returns a short value."
  [^InputStream stream]
  (let [bb (bb/allocate-lsb-byte-buffer 2)]
    (read-bytes stream (.array bb) 0 2)
    (.getShort bb)))

(defn read-ushort
  "Reads 2 bytes. Returns an unsigned short value as long."
  [^InputStream stream]
  (bit-and (short (read-short stream)) 0xFFFF))

(defn read-int
  "Reads 4 bytes. Returns an int value."
  [^InputStream stream]
  (let [bb (bb/allocate-lsb-byte-buffer 4)]
    (read-bytes stream (.array bb) 0 4)
    (.getInt bb)))

(defn read-uint
  "Reads 4 bytes. Returns an unsigned int value as long."
  [^InputStream stream]
  (bit-and (int (read-int stream)) 0xFFFFFFFF))

(defn read-long
  "Reads 8 bytes. Returns a long value. "
  [^InputStream stream]
  (let [bb (bb/allocate-lsb-byte-buffer 8)]
    (read-bytes stream (.array bb) 0 8)
    (.getLong bb)))

(defn read-float
  "Reads 4 bytes. Returns a float value."
  [^InputStream stream]
  (Float/intBitsToFloat (read-int stream)))

(defn read-double
  "Reads 8 bytes. Returns a double value."
  [^InputStream stream]
  (Double/longBitsToDouble (read-long stream)))

(defn read-bytes
  "Reads 'length' bytes to buffer starting from offset bytes. Returns a new byte-array if called without buffer."
  ([^InputStream stream ^long length]
   (let [ba (byte-array length)]
     (read-bytes stream ba 0 length)))
  ([^InputStream stream buffer ^long offset ^long length]
   (loop [total-read 0]
     (when (< total-read length)
       (let [n (.read stream buffer (+ offset total-read) (- length total-read))]
         (if (neg? n)
           (throw (EOFException. "Premature EOF"))
           (recur (+ total-read n))))))
   buffer))

(defn read-string
  "Reads 'length' bytes. Returns a String."
  [^InputStream stream ^long length]
  (String. ^bytes (read-bytes stream length)))

(defn read-null-terminated-string
  "Reads until next null character. Returns a String without the null."
  [^InputStream stream]
  (with-open [baos (ByteArrayOutputStream. 32)]
    (loop []
      (let [b (.read stream)]
        (when-not (zero? b)
          (.write baos b)
          (recur))))
    (.toString baos)))

(defn write-ubyte
  "Writes 1 byte."
  [^OutputStream stream b]
  (let [bb (bb/allocate-lsb-byte-buffer)]
    (.putShort bb b)
    (.write stream (.array bb) 0 1)))

(defn write-char
  "Writes a 1-byte ascii character."
  [^OutputStream stream b]
  (let [bb (bb/allocate-lsb-byte-buffer)]
    (.putChar bb b)
    (.write stream (.array bb) 0 1)))

(defn write-short
  "Writes a 2-byte short value."
  [^OutputStream stream n]
  (let [bb (bb/allocate-lsb-byte-buffer)]
    (.putShort bb n)
    (.write stream (.array bb) 0 2)))

(defn write-ushort
  "Writes a 2-byte unsigned short value."
  [^OutputStream stream n]
  (let [bb (bb/allocate-lsb-byte-buffer)]
    (.putInt bb n)
    (.write stream (.array bb) 0 2)))

(defn write-int
  "Writes a 4-byte integer value."
  [^OutputStream stream n]
  (let [bb (bb/allocate-lsb-byte-buffer)]
    (.putInt bb n)
    (.write stream (.array bb) 0 4)))

(defn write-uint
  "Writes a 4-byte unsigned integer value."
  [^OutputStream stream n]
  (let [bb (bb/allocate-lsb-byte-buffer)]
    (.putInt bb (unchecked-int n))
    (.write stream (.array bb) 0 4)))

(defn write-long
  "Writes an 8-byte long value."
  [^OutputStream stream n]
  (let [bb (bb/allocate-lsb-byte-buffer)]
    (.putLong bb n)
    (.write stream (.array bb) 0 8)))

(defn write-float
  "Writes a 4-byte float value."
  [^OutputStream stream n]
  (let [bb (bb/allocate-lsb-byte-buffer)]
    (.putFloat bb n)
    (.write stream (.array bb) 0 4)))

(defn write-double
  "Writes a 8-byte double value."
  [^OutputStream stream n]
  (let [bb (bb/allocate-lsb-byte-buffer)]
    (.putDouble bb n)
    (.write stream (.array bb) 0 8)))

(defn write-bytes
  "Writes a byte-array."
  [^OutputStream stream ^bytes b]
  (.write stream b 0 (alength b)))

(defn write-string
  "Writes a string as a sequence of ascii characters."
  [^OutputStream stream s]
  (let [data-bytes (string->bytes s)]
    (.write stream data-bytes 0 (alength data-bytes))))

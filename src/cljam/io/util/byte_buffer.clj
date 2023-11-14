(ns cljam.io.util.byte-buffer
  (:refer-clojure :exclude [read-string])
  (:require [cljam.util :refer [string->bytes]])
  (:import [java.nio Buffer ByteBuffer ByteOrder]))

(defn make-lsb-byte-buffer
  "Creates a new little-endian byte buffer wrapping given data."
  ^ByteBuffer [^bytes data]
  (.order (ByteBuffer/wrap data) ByteOrder/LITTLE_ENDIAN))

(defn make-msb-byte-buffer
  "Creates a new big-endian byte buffer wrapping given data."
  ^ByteBuffer [^bytes data]
  (.order (ByteBuffer/wrap data) ByteOrder/BIG_ENDIAN))

(defn allocate-lsb-byte-buffer
  "Creates a new little-endian byte buffer with given capacity."
  (^ByteBuffer []
   (.order (ByteBuffer/allocate 8) ByteOrder/LITTLE_ENDIAN))
  (^ByteBuffer [size]
   (.order (ByteBuffer/allocate (int size)) ByteOrder/LITTLE_ENDIAN)))

(defn allocate-msb-byte-buffer
  "Creates a new big-endian byte buffer with given capacity."
  (^ByteBuffer []
   (.order (ByteBuffer/allocate 8) ByteOrder/BIG_ENDIAN))
  (^ByteBuffer [size]
   (.order (ByteBuffer/allocate (int size)) ByteOrder/BIG_ENDIAN)))

(defn skip
  "Skips over 'length' bytes of data, discarding the skipped bytes."
  [^ByteBuffer bb ^long length]
  (.position ^Buffer bb (+ (.position bb) length)))

(defn read-ubyte
  "Reads 1 byte. Returns an unsigned byte value as long."
  [^ByteBuffer bb]
  (bit-and (.get bb) 0xFF))

(defn read-ushort
  "Reads 2 bytes. Returns an unsigned short value as long."
  [^ByteBuffer bb]
  (bit-and (.getShort bb) 0xFFFF))

(defn read-uint
  "Reads 4 bytes. Returns an unsigned int value as long."
  [^ByteBuffer bb]
  (bit-and (.getInt bb) 0xFFFFFFFF))

(defn read-bytes
  "Reads 'length' bytes to buffer starting from offset bytes. Returns a new byte-array if called without buffer."
  ([^ByteBuffer bb ^long length]
   (let [ba (byte-array length)]
     (.get bb ba)
     ba))
  ([^ByteBuffer bb buffer ^long offset ^long length]
   (.get bb buffer (int offset) (int length))
   buffer))

(defn read-string
  "Reads 'length' bytes. Returns a String."
  [^ByteBuffer bb ^long length]
  (let [ba (byte-array length)]
    (.get bb ba)
    (String. ba)))

(defn read-null-terminated-string
  "Reads until next null character. Returns a String without the null."
  [^ByteBuffer bb]
  (let [start (.position bb)
        end (do (while (not (zero? (.get bb))))
                (.position bb))
        offset (.arrayOffset bb)]
    (String. (.array bb) (+ offset start) (dec (- end start)))))

(defn write-ubyte
  "Writes 1 byte."
  [^ByteBuffer bb b]
  (.put bb (unchecked-byte b)))

(defn write-ushort
  "Writes a 2-byte unsigned short value."
  [^ByteBuffer bb n]
  (.putShort bb (unchecked-short n)))

(defn write-uint
  "Writes a 4-byte unsigned integer value."
  [^ByteBuffer bb n]
  (.putInt bb (unchecked-int n)))

(defn write-string
  "Writes a string as a sequence of ascii characters."
  [^ByteBuffer bb s]
  (.put bb (string->bytes s)))

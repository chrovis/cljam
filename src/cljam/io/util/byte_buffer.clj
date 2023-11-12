(ns cljam.io.util.byte-buffer
  {:clj-kondo/ignore [:missing-docstring]}
  (:refer-clojure :exclude [read-string])
  (:require [cljam.util :refer [string->bytes]])
  (:import [java.nio Buffer ByteBuffer ByteOrder]))

(defn make-lsb-byte-buffer ^ByteBuffer [^bytes data]
  (.order (ByteBuffer/wrap data) ByteOrder/LITTLE_ENDIAN))

(defn make-msb-byte-buffer ^ByteBuffer [^bytes data]
  (.order (ByteBuffer/wrap data) ByteOrder/BIG_ENDIAN))

(defn allocate-lsb-byte-buffer
  (^ByteBuffer []
   (.order (ByteBuffer/allocate 8) ByteOrder/LITTLE_ENDIAN))
  (^ByteBuffer [size]
   (.order (ByteBuffer/allocate (int size)) ByteOrder/LITTLE_ENDIAN)))

(defn allocate-msb-byte-buffer
  (^ByteBuffer []
   (.order (ByteBuffer/allocate 8) ByteOrder/BIG_ENDIAN))
  (^ByteBuffer [size]
   (.order (ByteBuffer/allocate (int size)) ByteOrder/BIG_ENDIAN)))

(defn skip [^ByteBuffer bb ^long length]
  (.position ^Buffer bb (+ (.position bb) length)))

(defn read-ubyte [^ByteBuffer bb]
  (bit-and (.get bb) 0xFF))

(defn read-ushort [^ByteBuffer bb]
  (bit-and (.getShort bb) 0xFFFF))

(defn read-uint [^ByteBuffer bb]
  (bit-and (.getInt bb) 0xFFFFFFFF))

(defn read-bytes
  ([^ByteBuffer bb ^long length]
   (let [ba (byte-array length)]
     (.get bb ba)
     ba))
  ([^ByteBuffer bb buffer ^long offset ^long length]
   (.get bb buffer (int offset) (int length))
   buffer))

(defn read-string [^ByteBuffer bb ^long length]
  (let [ba (byte-array length)]
    (.get bb ba)
    (String. ba)))

(defn read-null-terminated-string [^ByteBuffer bb]
  (let [start (.position bb)
        end (do (while (not (zero? (.get bb))))
                (.position bb))
        offset (.arrayOffset bb)]
    (String. (.array bb) (+ offset start) (dec (- end start)))))

(defn write-ubyte [^ByteBuffer bb b]
  (.put bb (unchecked-byte b)))

(defn write-ushort [^ByteBuffer bb n]
  (.putShort bb (unchecked-short n)))

(defn write-uint [^ByteBuffer bb n]
  (.putInt bb (unchecked-int n)))

(defn write-string [^ByteBuffer bb s]
  (.put bb (string->bytes s)))

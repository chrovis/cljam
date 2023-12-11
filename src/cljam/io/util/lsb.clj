(ns cljam.io.util.lsb
  "Reading/writing functions of stream and buffer for little-endian data."
  {:deprecated "0.8.5"}
  (:refer-clojure :exclude [read-string])
  (:require [cljam.util :refer [string->bytes]])
  (:import [java.io DataInput InputStream DataOutputStream EOFException ByteArrayOutputStream]
           [java.nio Buffer ByteBuffer ByteOrder]))

(defn gen-byte-buffer
  "Generates a new `java.nio.ByteBuffer` instance with little-endian byte order.
  The default buffer size is 8."
  (^ByteBuffer []
   (.order (ByteBuffer/allocate 8) ByteOrder/LITTLE_ENDIAN))
  (^ByteBuffer [size]
   (.order (ByteBuffer/allocate size) ByteOrder/LITTLE_ENDIAN)))

;; Reading
;; -------

(defprotocol LSBReadable
  "Provides feature of reading little-endian values."
  (skip [this length] "Skips over 'length' bytes of data, discarding the skipped bytes.")
  (read-byte [this] "Reads 1 byte. Returns a byte value.")
  (read-ubyte [this] "Reads 1 byte. Returns an unsigned byte value as long.")
  (read-short [this] "Reads 2 bytes. Returns a short value.")
  (read-ushort [this] "Reads 2 bytes. Returns an unsigned short value as long.")
  (read-int [this] "Reads 4 bytes. Returns an int value.")
  (read-uint [this] "Reads 4 bytes. Returns an unsigned int value as long.")
  (read-long [this] "Reads 8 bytes. Returns a long value. ")
  (read-float [this] "Reads 4 bytes. Returns a float value.")
  (read-double [this] "Reads 8 bytes. Returns a double value.")
  (read-bytes [this length] [this buffer offset length]
    "Reads 'length' bytes to buffer starting from offset bytes. Returns a new byte-array if called without buffer.")
  (read-string [this length] "Reads 'length' bytes. Returns a String.")
  (read-null-terminated-string [this] "Reads until next null character. Returns a String without the null."))

(extend-type ByteBuffer
  LSBReadable
  (skip [this ^long length]
    (.position ^Buffer this (+ (.position this) length)))
  (read-byte [this]
    (.get this))
  (read-ubyte [this]
    (bit-and (.get this) 0xFF))
  (read-short [this]
    (.order this ByteOrder/LITTLE_ENDIAN)
    (.getShort this))
  (read-ushort [this]
    (.order this ByteOrder/LITTLE_ENDIAN)
    (bit-and (.getShort this) 0xFFFF))
  (read-int [this]
    (.order this ByteOrder/LITTLE_ENDIAN)
    (.getInt this))
  (read-uint [this]
    (.order this ByteOrder/LITTLE_ENDIAN)
    (bit-and (.getInt this) 0xFFFFFFFF))
  (read-long [this]
    (.order this ByteOrder/LITTLE_ENDIAN)
    (.getLong this))
  (read-float [this]
    (.order this ByteOrder/LITTLE_ENDIAN)
    (.getFloat this))
  (read-double [this]
    (.order this ByteOrder/LITTLE_ENDIAN)
    (.getDouble this))
  (read-bytes
    ([this ^long length]
     (let [ba (byte-array length)]
       (.get this ba)
       ba))
    ([this ^bytes buffer ^long offset ^long length]
     (.get this buffer (int offset) (int length))
     buffer))
  (read-string [this ^long length]
    (let [ba (byte-array length)]
      (.get this ba)
      (String. ba)))
  (read-null-terminated-string [this]
    (let [start (.position this)
          end (do (while (not (zero? (.get this))))
                  (.position this))
          offset (.arrayOffset this)]
      (String. (.array this) (+ offset start) (dec (- end start))))))

(extend-type DataInput
  LSBReadable
  (skip [this ^long length]
    (.skipBytes this length))
  (read-byte [this]
    (.readByte this))
  (read-ubyte [this]
    (.readUnsignedByte this))
  (read-short [this]
    (let [bb (gen-byte-buffer 2)]
      (.readFully this (.array bb))
      (.getShort bb)))
  (read-ushort [this]
    (bit-and (short (read-short this)) 0xFFFF))
  (read-int [this]
    (let [bb (gen-byte-buffer 4)]
      (.readFully this (.array bb))
      (.getInt bb)))
  (read-uint [this]
    (bit-and (int (read-int this)) 0xFFFFFFFF))
  (read-long [this]
    (let [bb (gen-byte-buffer 8)]
      (.readFully this (.array bb))
      (.getLong bb)))
  (read-float [this]
    (Float/intBitsToFloat (read-int this)))
  (read-double [this]
    (Double/longBitsToDouble (read-long this)))
  (read-bytes
    ([this ^long length]
     (let [ba (byte-array length)]
       (.readFully this ba)
       ba))
    ([this buffer ^long offset ^long length]
     (.readFully this buffer offset length)
     buffer))
  (read-string [this ^long length]
    (let [ba (byte-array length)]
      (.readFully this ba)
      (String. ba)))
  (read-null-terminated-string [this]
    (with-open [baos (ByteArrayOutputStream. 32)]
      (loop []
        (let [b (.readByte this)]
          (when-not (zero? b)
            (.write baos b)
            (recur))))
      (.toString baos))))

(extend-type InputStream
  LSBReadable
  (skip [this ^long length]
    (.skip this length))
  (read-byte [this]
    (unchecked-byte (.read this)))
  (read-ubyte [this]
    (bit-and (.read this) 0xFF))
  (read-short [this]
    (let [bb (gen-byte-buffer 2)]
      (read-bytes this (.array bb) 0 2)
      (.getShort bb)))
  (read-ushort [this]
    (bit-and (short (read-short this)) 0xFFFF))
  (read-int [this]
    (let [bb (gen-byte-buffer 4)]
      (read-bytes this (.array bb) 0 4)
      (.getInt bb)))
  (read-uint [this]
    (bit-and (int (read-int this)) 0xFFFFFFFF))
  (read-long [this]
    (let [bb (gen-byte-buffer 8)]
      (read-bytes this (.array bb) 0 8)
      (.getLong bb)))
  (read-float [this]
    (Float/intBitsToFloat (read-int this)))
  (read-double [this]
    (Double/longBitsToDouble (read-long this)))
  (read-bytes
    ([this ^long length]
     (let [ba (byte-array length)]
       (read-bytes this ba 0 length)))
    ([this buffer ^long offset ^long length]
     (loop [total-read 0]
       (when (< total-read length)
         (let [n (.read this buffer (+ offset total-read) (- length total-read))]
           (if (neg? n)
             (throw (EOFException. "Premature EOF"))
             (recur (+ total-read n))))))
     buffer))
  (read-string [this ^long length]
    (String. ^bytes (read-bytes this length)))
  (read-null-terminated-string [this]
    (with-open [baos (ByteArrayOutputStream. 32)]
      (loop []
        (let [b (.read this)]
          (when-not (zero? b)
            (.write baos b)
            (recur))))
      (.toString baos))))

;; Writing
;; -------

(defprotocol LSBWritable
  "Provides feature of writing little-endian values."
  (write-ubyte [this n] "Writes 1 byte.")
  (write-char [this n] "Writes a 1-byte ascii character.")
  (write-short [this n] "Writes a 2-byte short value.")
  (write-ushort [this n] "Writes a 2-byte unsigned short value.")
  (write-int [this n] "Writes a 4-byte integer value.")
  (write-uint [this n] "Writes a 4-byte unsigned integer value.")
  (write-long [this n] "Writes an 8-byte long value.")
  (write-float [this n] "Writes a 4-byte float value.")
  (write-bytes [this b] "Writes a byte-array.")
  (write-string [this s] "Writes a string as a sequence of ascii characters."))

(extend-type ByteBuffer
  LSBWritable
  (write-ubyte [w b]
    (.put w (unchecked-byte b)))
  (write-char [w b]
    (.put w (unchecked-byte (long b))))
  (write-short [w n]
    (.order w ByteOrder/LITTLE_ENDIAN)
    (.putShort w n))
  (write-ushort [w n]
    (.order w ByteOrder/LITTLE_ENDIAN)
    (.putShort w (unchecked-short n)))
  (write-int [w n]
    (.order w ByteOrder/LITTLE_ENDIAN)
    (.putInt w n))
  (write-uint [w n]
    (.order w ByteOrder/LITTLE_ENDIAN)
    (.putInt w (unchecked-int n)))
  (write-long [w n]
    (.order w ByteOrder/LITTLE_ENDIAN)
    (.putLong w n))
  (write-float [w n]
    (.order w ByteOrder/LITTLE_ENDIAN)
    (.putFloat w n))
  (write-bytes [w ^bytes b]
    (.put w b))
  (write-string [w s]
    (.put w (string->bytes s))))

(extend-type DataOutputStream
  LSBWritable
  (write-ubyte [w b]
    (let [bb (gen-byte-buffer)]
      (.putShort bb b)
      (.write w (.array bb) 0 1)))
  (write-char [w b]
    (let [bb (gen-byte-buffer)]
      (.putChar bb b)
      (.write w (.array bb) 0 1)))
  (write-short [w n]
    (let [bb (gen-byte-buffer)]
      (.putShort bb n)
      (.write w (.array bb) 0 2)))
  (write-ushort [w n]
    (let [bb (gen-byte-buffer)]
      (.putInt bb n)
      (.write w (.array bb) 0 2)))
  (write-int [w n]
    (let [bb (gen-byte-buffer)]
      (.putInt bb n)
      (.write w (.array bb) 0 4)))
  (write-uint [w n]
    (let [bb (gen-byte-buffer)]
      (.putInt bb (unchecked-int n))
      (.write w (.array bb) 0 4)))
  (write-long [w n]
    (let [bb (gen-byte-buffer)]
      (.putLong bb n)
      (.write w (.array bb) 0 8)))
  (write-float [w n]
    (let [bb (gen-byte-buffer)]
      (.putFloat bb n)
      (.write w (.array bb) 0 4)))
  (write-bytes [w ^bytes b]
    (.write w b 0 (alength b)))
  (write-string [w s]
    (let [data-bytes (string->bytes s)]
      (.write w data-bytes 0 (alength data-bytes)))))

(extend-type java.nio.channels.FileChannel
  LSBWritable
  (write-ubyte [w b]
    (let [bb (-> (gen-byte-buffer 1) (.put (unchecked-byte b)))]
      (.clear ^Buffer bb)
      (.write w bb)))
  (write-char [w b]
    (let [bb (-> (gen-byte-buffer 1) (.putChar b))]
      (.clear ^Buffer bb)
      (.write w bb)))
  (write-short [w n]
    (let [bb (-> (gen-byte-buffer 2) (.putShort n))]
      (.clear ^Buffer bb)
      (.write w bb)))
  (write-ushort [w n]
    (let [bb (-> (gen-byte-buffer 2) (.putShort (unchecked-short n)))]
      (.clear ^Buffer bb)
      (.write w bb)))
  (write-int [w n]
    (let [bb (-> (gen-byte-buffer 4) (.putInt n))]
      (.clear ^Buffer bb)
      (.write w bb)))
  (write-uint [w n]
    (let [bb (-> (gen-byte-buffer 4) (.putInt (unchecked-int n)))]
      (.clear ^Buffer bb)
      (.write w bb)))
  (write-long [w n]
    (let [bb (-> (gen-byte-buffer 8) (.putLong n))]
      (.clear ^Buffer bb)
      (.write w bb)))
  (write-float [w n]
    (let [bb (-> (gen-byte-buffer 4) (.putFloat n))]
      (.clear ^Buffer bb)
      (.write w bb)))
  (write-bytes [w ^bytes b]
    (.write w (ByteBuffer/wrap b)))
  (write-string [w s]
    (.write w (ByteBuffer/wrap (string->bytes s)))))

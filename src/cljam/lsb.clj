(ns cljam.lsb
  (:refer-clojure :exclude [read-string])
  (:use [cljam.util :only [string->bytes bytes->string]])
  (:import [java.nio ByteBuffer ByteOrder]))

(defn- gen-byte-buffer []
  (.order (ByteBuffer/allocate 8) ByteOrder/LITTLE_ENDIAN))

;;; reading

(defn read-bytes
  ([r l]
     (let [ba (byte-array l)]
       (.read r ba 0 l)
       ba))
  ([r buffer offset l]
     (loop [total-read 0]
       (if (>= total-read l) nil
           (do
             (let [num-read (.read r buffer (+ offset total-read) (- l total-read))]
               (if (neg? num-read)
                 (throw (Exception. "Premature EOF"))
                 (recur (+ total-read num-read)))))))))

(defn- read-byte-buffer [r bb l]
  {:pre (< l (.capacity bb))}
  (read-bytes r (.array bb) 0 l)
  (.limit bb (.capacity bb))
  (.position bb l))

(defn read-ubyte [r]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer r bb 1)
    (.put bb (byte 0))
    (.flip bb)
    (.getShort bb)))

(defn read-short [r]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer r bb 2)
    (.flip bb)
    (.getShort bb)))

(defn read-ushort [r]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer r bb 2)
    (.putShort bb (short 0))
    (.flip bb)
    (.getInt bb)))

(defn read-int [r]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer r bb 4)
    (.flip bb)
    (.getInt bb)))

(defn read-float [r]
  (let [bb (gen-byte-buffer)]
    (read-byte-buffer r bb 4)
    (.flip bb)
    (.getFloat bb)))

(defn read-string [r l]
  (String. (read-bytes r l) 0 0 l))

(defn read-null-terminated-string [bb]
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

(defn write-bytes [w b]
  (.write w b 0 (count b))
  nil)

(defn write-ubyte [w b]
  (let [bb (gen-byte-buffer)]
    (.putShort bb b)
    (.write w (.array bb) 0 1)
    nil))

(defn write-short [w n]
  (let [bb (gen-byte-buffer)]
    (.putShort bb n)
    (.write w (.array bb) 0 2)
    nil))

(defn write-ushort [w n]
  (let [bb (gen-byte-buffer)]
    (.putInt bb n)
    (.write w (.array bb) 0 2)
    nil))

(defn write-int [w n]
  (let [bb (gen-byte-buffer)]
    (.putInt bb n)
    (.write w (.array bb) 0 4)
    nil))

(defn write-float [w n]
  (let [bb (gen-byte-buffer)]
    (.putFloat bb n)
    (.write w (.array bb) 0 4)
    nil))

(defn write-string [w s]
  (let [data-bytes (string->bytes s)]
   (.write w data-bytes 0 (count data-bytes))
   nil))

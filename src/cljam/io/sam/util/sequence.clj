(ns cljam.io.sam.util.sequence
  "Utility functions for base sequences."
  (:require [clojure.string :as cstr])
  (:import [java.nio ByteBuffer CharBuffer]))

(def ^:private ^:const nibble-to-base-table
  ;; Index: nibble of a compressed base.
  ;; Value: base for the nibble.
  "=ACMGRSVTWYHKDBN")

(def ^:private two-bytes-to-compressed-bases-table
  ;; Index: two bases (A,C) => ASCII (65,67) => 2r 1000001 1000011 => 8387
  ;; Value: two bases (A,C) => nibbles (1,2) => 2r 0001 0010 => 18
  (let [ba (byte-array (bit-shift-left 1 14))
        byte-to-nibble-table (byte-array (bit-shift-left 1 7) (byte 15))]
    (doseq [[i c] (map vector (range) nibble-to-base-table)]
      (aset-byte byte-to-nibble-table (int c) i)
      (aset-byte byte-to-nibble-table (int (.charAt (cstr/lower-case c) 0)) i))
    (dotimes [i (alength ba)]
      (let [u (unchecked-byte (bit-and 0x7F (unsigned-bit-shift-right i 7)))
            l (unchecked-byte (bit-and 0x7F i))]
        (->> (aget byte-to-nibble-table l)
             (bit-or (bit-shift-left (aget byte-to-nibble-table u) 4))
             unchecked-byte
             (aset-byte ba i))))
    ba))

(defn str->compressed-bases
  "Creates a buffer consists of compressed bases from ASCII sequence."
  ^bytes [^String s]
  (let [b (.getBytes s)
        length (alength b)
        result-len (quot (inc length) 2)
        in-bb (ByteBuffer/wrap b)
        out-bb (ByteBuffer/allocate result-len)]
    (dotimes [i result-len]
      (let [u (.get in-bb)
            l (byte (if (.hasRemaining in-bb) (.get in-bb) \=))]
        (->> (bit-and 0x7F l)
             (bit-or (bit-shift-left (bit-and 0x7F u) 7))
             (aget ^bytes two-bytes-to-compressed-bases-table)
             (.put out-bb))))
    (.array out-bb)))

(def ^:const ^:private compressed-bases-to-bases-table
  ;; Index: compressed base n containing two nibbles => 2n
  ;; Value 2n+0: base for upper nibble of n.
  ;; Value 2n+1: base for lower nibble of n.
  (->> (for [i nibble-to-base-table j nibble-to-base-table] [i j])
       (apply concat)
       cstr/join))

(defn compressed-bases->str
  "Decode a sequence from byte array to String."
  [^long length ^bytes compressed-bases ^long compressed-offset]
  (let [cb (CharBuffer/allocate (inc length))
        bb (ByteBuffer/wrap compressed-bases)]
    (.position bb compressed-offset)
    (dotimes [_ (quot (inc length) 2)]
      (let [i (-> (.get bb) (bit-and 0xff) (* 2))]
        (.put cb (.charAt compressed-bases-to-bases-table i))
        (.put cb (.charAt compressed-bases-to-bases-table (inc i)))))
    (.limit cb length)
    (.flip cb)
    (.toString cb)))

(defn normalize-bases
  "Converts bases in given buffer to upper-case. Also converts '.' to 'N'.
   Bases are represented as buffer of ASCII characters."
  ^bytes [^bytes bases]
  (dotimes [i (alength bases)]
    (let [b (aget bases i)]
      (cond
        (= b (byte \.)) (aset-byte bases i (byte \N))
        (<= (byte \a) b (byte \z)) (aset-byte bases i (- b 32))))) ;; Upper-case ASCII offset
  bases)

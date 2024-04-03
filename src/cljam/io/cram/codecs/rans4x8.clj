(ns cljam.io.cram.codecs.rans4x8
  (:require [cljam.io.util.byte-buffer :as bb]
            [cljam.io.cram.itf8 :as itf8])
  (:import [java.util Arrays]))

(def ^:private byte-array-type (type (byte-array 0)))
(def ^:private int-array-type (type (int-array 0)))

(defmacro ^:private read-frequencies* [bb init-expr read-expr]
  `(let [ret# ~init-expr]
     (loop [sym# (long (bb/read-ubyte ~bb))
            rle# 0]
       (aset ret# sym# ~read-expr)
       (if (pos? rle#)
         (recur (inc sym#) (dec rle#))
         (let [sym'# (long (bb/read-ubyte ~bb))
               rle'# (if (= sym'# (inc sym#))
                       (long (bb/read-ubyte ~bb))
                       rle#)]
           (if (zero? sym'#)
             ret#
             (recur sym'# rle'#)))))))

(defn- read-frequencies0 ^ints [bb]
  (read-frequencies* bb (int-array 256) (int (itf8/decode-itf8 bb))))

(defn- read-frequencies1 ^"[[I" [bb]
  (read-frequencies* bb ^"[[I" (make-array int-array-type 256) (read-frequencies0 bb)))

(def ^:private zero-int-array (int-array 256))

(defn- cumulative-frequencies ^ints [^ints freqs]
  (if (nil? freqs)
    zero-int-array
    (let [cum-freqs (int-array 256)]
      (loop [i 0
             sum 0]
        (when (< i 256)
          (let [f (aget freqs i)]
            (aset cum-freqs i sum)
            (recur (inc i) (+ sum (long f))))))
      cum-freqs)))

(defn- reverse-lookup-table ^bytes [^ints cum-freqs]
  (let [arr (byte-array 4096)
        n (alength cum-freqs)]
    (loop [i 1, start 0]
      (if (< i n)
        (let [curr (aget cum-freqs i)]
          (if (= start curr)
            (recur (inc i) start)
            (do (Arrays/fill arr start curr (byte (dec i)))
                (recur (inc i) curr))))
        (Arrays/fill arr start 4096 (byte 255))))
    arr))

(defn- advance-step ^long [^long c ^long f ^long state]
  (-> (* f (bit-shift-right state 12))
      (+ (bit-and state 0xfff))
      (- c)))

(defn- renormalize-state ^long [bb ^long state]
  (loop [state state]
    (if (< state 0x800000)
      (recur (bit-or (bit-shift-left state 8) (long (bb/read-ubyte bb))))
      state)))

(defn- decode0 [bb ^long n-out]
  (let [freqs (read-frequencies0 bb)
        cum-freqs (cumulative-frequencies freqs)
        table (reverse-lookup-table cum-freqs)
        states (bb/read-ints bb 4)
        out (byte-array n-out)]
    (dotimes [i n-out]
      (let [j (rem i 4)
            state (aget states j)
            f (bit-and state 0xfff)
            sym (bit-and (aget table f) 0xff)
            state' (->> state
                        (advance-step (aget cum-freqs sym) (aget freqs sym))
                        (renormalize-state bb))]
        (aset out i (byte sym))
        (aset states j state')))
    out))

(defn- decode1 [bb ^long n-out]
  (let [freqs (read-frequencies1 bb)
        ^"[[I" cum-freqs (make-array int-array-type 256)
        _ (dotimes [i 256]
            (aset cum-freqs i (cumulative-frequencies (aget freqs i))))
        ^"[[B" tables (make-array byte-array-type 256)
        _ (dotimes [i 256]
            (aset tables i (reverse-lookup-table (aget cum-freqs i))))
        quarter (quot n-out 4)
        truncated (* 4 quarter)
        states (bb/read-ints bb 4)
        last-syms (int-array 4)
        out (byte-array n-out)]
    (dotimes [i quarter]
      (dotimes [j 4]
        (let [state (aget states j)
              f (bit-and state 0xfff)
              last-sym (aget last-syms j)
              ^ints cfreqs (aget cum-freqs last-sym)
              sym (bit-and (aget ^bytes (aget tables last-sym) f) 0xff)
              state' (->> state
                          (advance-step (aget cfreqs sym)
                                        (aget ^ints (aget freqs last-sym) sym))
                          (renormalize-state bb))]
          (aset out (+ i (* j quarter)) (byte sym))
          (aset states j state')
          (aset last-syms j sym))))
    (dotimes [i (- n-out truncated)]
      (let [state (aget states 3)
            f (bit-and state 0xfff)
            last-sym (aget last-syms 3)
            ^ints cfreq (aget cum-freqs last-sym)
            sym (bit-and (aget ^bytes (aget tables last-sym) f) 0xff)
            state' (->> state
                        (advance-step (aget cfreq sym)
                                      (aget ^ints (aget freqs last-sym) sym))
                        (renormalize-state bb))]
        (aset out (+ i truncated) (byte sym))
        (aset states 3 state')
        (aset last-syms 3 sym)))
    out))

(defn decode
  "Reads a byte sequence from the given ByteBuffer and decodes it by the rANS4x8 codec.
  Returns the decoded result as a byte array."
  ^bytes [bb]
  (let [order (long (bb/read-ubyte bb))
        _n-in (bb/read-uint bb)
        n-out (bb/read-uint bb)]
    (if (zero? order)
      (decode0 bb n-out)
      (decode1 bb n-out))))

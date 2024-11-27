(ns cljam.io.cram.codecs.rans4x8
  (:refer-clojure :exclude [reset!])
  (:require [cljam.io.cram.itf8 :as itf8]
            [cljam.io.util.byte-buffer :as bb])
  (:import [java.nio ByteBuffer]
           [java.util Arrays]))

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

(defn- calculate-frequencies0 ^"[B" [^ByteBuffer bb]
  (let [size (.remaining bb)
        freqs (int-array 256)
        _ (dotimes [_ size]
            (let [b (long (bb/read-ubyte bb))]
              (aset freqs b (inc (aget freqs b)))))
        tr (+ (quot (bit-shift-left 4096 31) size)
              (quot (bit-shift-left 1 31) size))
        max-idx (loop [i 0, cur 0, m 0]
                  (if (< i 256)
                    (let [f (aget freqs i)]
                      (if (<= f m)
                        (recur (inc i) cur m)
                        (recur (inc i) i f)))
                    cur))
        fsum (loop [i 0, fsum 0]
               (if (< i 256)
                 (let [f (aget freqs i)]
                   (if-not (zero? f)
                     (let [f' (unsigned-bit-shift-right (* f tr) 31)]
                       (aset freqs f (if (zero? f') 1 f'))
                       (recur (inc i) (+ fsum f')))
                     (recur (inc i) fsum)))
                 (inc fsum)))]
    (aset freqs max-idx
          (if (< fsum 4096)
            (+ (aget freqs max-idx) (- 4096 fsum))
            (- (aget freqs max-idx) (- fsum 4096))))
    freqs))

(defn- write-frequencies0 [^ByteBuffer out ^"[I" freqs]
  (let [start (.position out)]
    (loop [i 0, rle 0]
      (when (< i 256)
        (let [f (aget freqs i)]
          (when-not (zero? f)
            (let [rle' (if (zero? rle)
                         (do (.put out (byte i))
                             (when (and (> i 0) (not (zero? (aget freqs (dec i)))))
                               (let [rle' (loop [rle (inc i)]
                                            (if (and (< rle 256)
                                                     (not (zero? (aget freqs rle))))
                                              (recur (inc rle))
                                              (- rle (inc i))))]
                                 (.put out (byte rle'))
                                 rle')))
                         (dec rle))]
              (if (< f 128)
                (.put out (byte f))
                (do (.put out (bit-or 128 (unsigned-bit-shift-right f 8)))
                    (.put out (bit-and 0xff f))))
              (recur (inc i) rle'))))))
    (.put out 0)
    (- (.position out) start)))

(defprotocol ISymbolState
  (reset! [this])
  (init! [this start freq scale-bits])
  (update! [^ByteBuffer bb b]))

(deftype SymbolState
         [^:unsynchronized-mutable ^long max
          ^:unsynchronized-mutable ^long rcp-freq
          ^:unsynchronized-mutable ^long bias
          ^:unsynchronized-mutable ^long cmpl-freq
          ^:unsynchronized-mutable ^long rcp-shift]
  ISymbolState
  (reset! [_]
    (set! max 0)
    (set! rcp-freq 0)
    (set! bias 0)
    (set! cmpl-freq 0)
    (set! rcp-shift 0))
  (init! [_ start freq]
    (set! max (* (bit-shift-left 1 19) freq))
    (set! cmpl-freq (- (bit-shift-left 1 12) freq))
    (if (< freq 2)
      (do (set! rcp-freq (bit-not 0))
          (set! rcp-shift 0)
          (set! bias (dec (+ start (bit-shift-left 1 12)))))
      (let [shift (loop [shift 0]
                    (if (< (bit-shift-left 1 shift) freq)
                      (recur (inc shift))
                      shift))]
        (set! rcp-freq (quot (dec (+ (bit-shift-left 1 (+ shift 31)) freq)) freq))
        (set! rcp-shift (dec shift))
        (set! bias start)))
    (set! rcp-shift (+ rcp-shift 32)))
  (update! [_ bb b]
    (let [x (loop [i 2, x b]
              (if (or (zero? i) (< x max))
                x
                (do (.put bb (byte (bit-and 0xff x)))
                    (recur (dec i) (unsigned-bit-shift-right x 8)))))
          q (unsigned-bit-shift-right (* x (bit-and 0xfffffff rcp-freq)) rcp-shift)]
      (+ x bias (* q cmpl-freq)))))

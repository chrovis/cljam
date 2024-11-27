(ns cljam.io.cram.codecs.rans4x8
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

(defn- calculate-frequencies0 ^"[I" [^ByteBuffer bb]
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
        fsum (long
              (loop [i 0, fsum 0]
                (if (< i 256)
                  (let [f (aget freqs i)]
                    (if (zero? f)
                      (recur (inc i) fsum)
                      (let [f' (unsigned-bit-shift-right (* f tr) 31)]
                        (aset freqs i (if (zero? f') 1 f'))
                        (recur (inc i) (+ fsum f')))))
                  (inc fsum))))]
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
          (if (zero? f)
            (recur (inc i) rle)
            (let [rle' (long
                        (if (zero? rle)
                          (do (.put out (byte i))
                              (if (and (> i 0) (not (zero? (aget freqs (dec i)))))
                                (let [rle' (loop [rle (inc i)]
                                             (if (and (< rle 256)
                                                      (not (zero? (aget freqs rle))))
                                               (recur (inc rle))
                                               (- rle (inc i))))]
                                  (.put out (byte rle'))
                                  rle')
                                rle))
                          (dec rle)))]
              (if (< f 128)
                (.put out (byte f))
                (do (.put out (byte (bit-or 128 (unsigned-bit-shift-right f 8))))
                    (.put out (byte (bit-and 0xff f)))))
              (recur (inc i) rle'))))))
    (.put out (byte 0))
    (- (.position out) start)))

(defprotocol ISymbolState
  (init! [this start freq])
  (update! [this ^ByteBuffer bb b]))

(deftype SymbolState
         [^:unsynchronized-mutable ^long max
          ^:unsynchronized-mutable ^long rcp-freq
          ^:unsynchronized-mutable ^long bias
          ^:unsynchronized-mutable ^long cmpl-freq
          ^:unsynchronized-mutable ^long rcp-shift]
  ISymbolState
  (init! [_ start freq]
    (let [start (long start)
          freq (long freq)]
      (set! max (* (bit-shift-left 1 19) freq))
      (set! cmpl-freq (- (bit-shift-left 1 12) freq))
      (if (< freq 2)
        (do (set! rcp-freq (bit-not 0))
            (set! rcp-shift 0)
            (set! bias (dec (+ start (bit-shift-left 1 12)))))
        (let [shift (long
                     (loop [shift 0]
                       (if (< (bit-shift-left 1 shift) freq)
                         (recur (inc shift))
                         shift)))]
          (set! rcp-freq (quot (dec (+ (bit-shift-left 1 (+ shift 31)) freq)) freq))
          (set! rcp-shift (dec shift))
          (set! bias start)))
      (set! rcp-shift (+ rcp-shift 32))))
  (update! [_ bb b]
    (let [x (long
             (loop [i 2, x (long b)]
               (if (or (zero? i) (< x max))
                 x
                 (do (.put ^ByteBuffer bb (byte (bit-and 0xff x)))
                     (recur (dec i) (unsigned-bit-shift-right x 8))))))
          q (unsigned-bit-shift-right (* x (bit-and 0xfffffff rcp-freq)) rcp-shift)]
      (+ x bias (* q cmpl-freq)))))

(def ^:private ^:const RANS_BYTE_L (bit-shift-left 1 23))

(defn- encode* [^ByteBuffer in ^objects syms ^ByteBuffer out]
  (let [raw-size (.remaining in)
        states (int-array 4 RANS_BYTE_L)
        frac (bit-and raw-size 3)]
    (when (= frac 3)
      (aset states 2
            (long (update! ^SymbolState (aget syms (bit-and 0xff (.get in (- raw-size frac -2))))
                           out RANS_BYTE_L))))
    (when (>= frac 2)
      (aset states 1
            (long (update! ^SymbolState (aget syms (bit-and 0xff (.get in (- raw-size frac -1))))
                           out RANS_BYTE_L))))
    (when (>= frac 1)
      (aset states 0
            (long (update! ^SymbolState (aget syms (bit-and 0xff (.get in (- raw-size frac))))
                           out RANS_BYTE_L))))
    (loop [left (bit-and raw-size (bit-not 3))]
      (when (> left 0)
        (let [c3 (bit-and 0xff (.get in (- left 1)))
              c2 (bit-and 0xff (.get in (- left 2)))
              c1 (bit-and 0xff (.get in (- left 3)))
              c0 (bit-and 0xff (.get in (- left 4)))]
          (aset states 3 (long (update! ^SymbolState (aget syms c3) out (aget states 3))))
          (aset states 2 (long (update! ^SymbolState (aget syms c2) out (aget states 2))))
          (aset states 1 (long (update! ^SymbolState (aget syms c1) out (aget states 1))))
          (aset states 0 (long (update! ^SymbolState (aget syms c0) out (aget states 0))))
          (recur (- left 4)))))
    (.putInt out (aget states 3))
    (.putInt out (aget states 2))
    (.putInt out (aget states 1))
    (.putInt out (aget states 0))
    (.flip out)
    (.position in (.limit in))
    (.limit out)))

(def ^:private ^:const ORDER_BYTE_LEN 1)
(def ^:private ^:const COMPRESSED_BYTE_LEN 4)
(def ^:private ^:const RAW_BYTE_LEN 4)
(def ^:private ^:const PREFIX_BYTE_LEN
  (+ ORDER_BYTE_LEN COMPRESSED_BYTE_LEN RAW_BYTE_LEN))

(defn- allocate-output-buffer ^ByteBuffer [raw-size]
  (let [compressed-size (+ (* 1.05 raw-size) (* 257 257 3) 9)]
    (bb/allocate-lsb-byte-buffer compressed-size)))

(defn encode
  "TODO"
  ^ByteBuffer [^ByteBuffer in]
  (let [raw-size (.remaining in)
        ^ByteBuffer out (doto (allocate-output-buffer raw-size)
                          (.position PREFIX_BYTE_LEN))
        freqs (calculate-frequencies0 in)
        syms (make-array SymbolState 256)
        cum-freqs (int-array 256)
        _ (loop [i 0, total 0]
            (when (< i 256)
              (aset syms i (->SymbolState 0 0 0 0 0))
              (aset cum-freqs i total)
              (let [f (aget freqs i)]
                (when (> f 0)
                  (init! ^SymbolState (aget syms i) total f))
                (recur (inc i) (+ total f)))))
        out' (.slice out)
        freq-table-size (write-frequencies0 out' freqs)
        _ (.rewind in)
        compressed-size (encode* in syms out')]
    (.limit out (+ PREFIX_BYTE_LEN freq-table-size compressed-size))
    (.put out (byte 0))
    (.putInt out ORDER_BYTE_LEN (+ freq-table-size compressed-size))
    (.putInt out (+ ORDER_BYTE_LEN COMPRESSED_BYTE_LEN) raw-size)
    (.rewind out)
    out))

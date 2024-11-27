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
                      (let [f' (as-> (unsigned-bit-shift-right (* f tr) 31) f'
                                 (if (zero? f') 1 f'))]
                        (aset freqs i f')
                        (recur (inc i) (+ fsum f')))))
                  (inc fsum))))]
    (aset freqs max-idx
          (if (< fsum 4096)
            (+ (aget freqs max-idx) (- 4096 fsum))
            (- (aget freqs max-idx) (- fsum 4096))))
    freqs))

(defn- calculate-frequencies1 ^"[[I" [^ByteBuffer bb]
  (let [size (.remaining bb)
        ^"[[I" freqs (make-array Integer/TYPE 256 256)
        sums (int-array 256)
        _ (loop [i 0, prev 0]
            (when (< i size)
              (let [b (long (bb/read-ubyte bb))
                    ^ints fs (aget freqs prev)]
                (aset fs b (inc (aget fs b)))
                (aset sums prev (inc (aget sums prev)))
                (recur (inc i) b))))
        ^ints f0 (aget freqs 0)
        b (bit-and 0xff (.get bb (unsigned-bit-shift-right size 2)))
        _ (aset f0 b (inc (aget f0 b)))
        b (bit-and 0xff (.get bb (* 2 (unsigned-bit-shift-right size 2))))
        _ (aset f0 b (inc (aget f0 b)))
        b (bit-and 0xff (.get bb (* 3 (unsigned-bit-shift-right size 2))))
        _ (aset f0 b (inc (aget f0 b)))]
    (aset sums 0 (+ (aget sums 0) 3))
    (loop [i 0]
      (when (< i 256)
        (let [sum (aget sums i)]
          (if (zero? sum)
            (recur (inc i))
            (let [p (/ 4096.0 sum)
                  ^ints fs (aget freqs i)]
              (loop [j 0, m 0, M 0, fsum 0]
                (if (< j 256)
                  (let [f (aget fs j)]
                    (if (zero? f)
                      (recur (inc j) m M fsum)
                      (let [f' (as-> (long (* f p)) f'
                                 (if (zero? f') 1 f'))
                            fsum' (+ fsum f')]
                        (aset fs j f')
                        (if (< m f')
                          (recur (inc j) f' j fsum')
                          (recur (inc j) m M fsum')))))
                  (let [f (aget fs M)
                        fsum' (inc fsum)]
                    (if (< fsum' 4096)
                      (aset fs M (+ f (- 4096 fsum')))
                      (aset fs M (- f (- fsum' 4096)))))))
              (recur (inc i)))))))
    freqs))

(defn- write-frequencies0 ^long [^ByteBuffer out ^ints freqs]
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

(defn- write-frequencies1 [^ByteBuffer out ^"[[I" freqs]
  (let [start (.position out)
        sums (int-array 256)
        _ (dotimes [i 256]
            (dotimes [j 256]
              (aset sums i (+ (aget sums i) (aget ^ints (aget freqs i) j)))))]
    (loop [i 0, rle-i 0]
      (when (< i 256)
        (if (zero? (aget sums i))
          (recur (inc i) rle-i)
          (let [rle-i' (long
                        (if (zero? rle-i)
                          (do (.put out (byte i))
                              (if (and (> i 0) (not (zero? (aget sums (dec i)))))
                                (let [rle-i' (loop [rle-i (inc i)]
                                               (if (and (< rle-i 256)
                                                        (not (zero? (aget sums rle-i))))
                                                 (recur (inc rle-i))
                                                 (- rle-i (inc i))))]
                                  (.put out (byte rle-i'))
                                  rle-i')
                                rle-i))
                          (dec rle-i)))
                ^ints fs (aget freqs i)]
            (loop [j 0, rle-j 0]
              (when (< j 256)
                (let [f (aget fs j)]
                  (if (zero? f)
                    (recur (inc j) rle-j)
                    (let [rle-j' (long
                                  (if (zero? rle-j)
                                    (do (.put out (byte j))
                                        (if (and (> j 0) (not (zero? (aget fs (dec j)))))
                                          (let [rle-j' (loop [rle-j (inc j)]
                                                         (if (and (< rle-j 256)
                                                                  (not (zero? (aget fs rle-j))))
                                                           (recur (inc rle-j))
                                                           (- rle-j (inc j))))]
                                            (.put out (byte rle-j'))
                                            rle-j')
                                          rle-j))
                                    (dec rle-j)))]
                      (if (< f 128)
                        (.put out (byte f))
                        (do (.put out (byte (bit-or 128 (unsigned-bit-shift-right f 8))))
                            (.put out (byte (bit-and 0xff f)))))
                      (recur (inc j) rle-j'))))))
            (.put out (byte 0))
            (recur (inc i) rle-i')))))
    (.put out (byte 0))
    (- (.position out) start)))

(defprotocol ISymbolState
  (init! [this start freq])
  (update! [this ^ByteBuffer bb b]))

(deftype SymbolState
         [^:unsynchronized-mutable ^long xmax
          ^:unsynchronized-mutable ^long rcp-freq
          ^:unsynchronized-mutable ^long bias
          ^:unsynchronized-mutable ^long cmpl-freq
          ^:unsynchronized-mutable ^long rcp-shift]
  ISymbolState
  (init! [_ start freq]
    (let [start (long start)
          freq (long freq)]
      (set! xmax (* (bit-shift-left 1 19) freq))
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
  (update! [_ bb r]
    (let [x (long
             (loop [i 2, x (int r)]
               (if (or (zero? i) (< x xmax))
                 x
                 (do (.put ^ByteBuffer bb (byte (bit-and 0xff x)))
                     (recur (dec i) (unsigned-bit-shift-right x 8))))))
          q (unsigned-bit-shift-right (* x (bit-and 0xffffffff rcp-freq)) rcp-shift)]
      (+ x bias (* q cmpl-freq)))))

(def ^:private ^:const RANS_BYTE_L (bit-shift-left 1 23))

(defn- reverse-buffer! [^ByteBuffer bb]
  (let [arr (.array bb)
        offset (.arrayOffset bb)
        size (.limit bb)]
    (loop [i offset, j (dec (+ offset size))]
      (when (< i j)
        (let [t (aget arr j)]
          (aset arr j (aget arr i))
          (aset arr i t)
          (recur (inc i) (dec j)))))))

(defn- encode0* ^long [^ByteBuffer in ^objects syms ^ByteBuffer out]
  (let [raw-size (.remaining in)
        states (int-array 4 RANS_BYTE_L)
        r (bit-and raw-size 3)
        out' (.slice out)]
    (when (= r 3)
      (aset states 2
            (int (update! ^SymbolState (aget syms (bit-and 0xff (.get in (- raw-size r -2))))
                          out' (aget states 2)))))
    (when (>= r 2)
      (aset states 1
            (int (update! ^SymbolState (aget syms (bit-and 0xff (.get in (- raw-size r -1))))
                          out' (aget states 1)))))
    (when (>= r 1)
      (aset states 0
            (int (update! ^SymbolState (aget syms (bit-and 0xff (.get in (- raw-size r))))
                          out' (aget states 0)))))
    (loop [left (bit-and raw-size (bit-not 3))]
      (when (> left 0)
        (let [c3 (bit-and 0xff (.get in (- left 1)))
              c2 (bit-and 0xff (.get in (- left 2)))
              c1 (bit-and 0xff (.get in (- left 3)))
              c0 (bit-and 0xff (.get in (- left 4)))]
          (aset states 3 (int (update! ^SymbolState (aget syms c3) out' (aget states 3))))
          (aset states 2 (int (update! ^SymbolState (aget syms c2) out' (aget states 2))))
          (aset states 1 (int (update! ^SymbolState (aget syms c1) out' (aget states 1))))
          (aset states 0 (int (update! ^SymbolState (aget syms c0) out' (aget states 0))))
          (recur (- left 4)))))
    (.putInt out' (aget states 3))
    (.putInt out' (aget states 2))
    (.putInt out' (aget states 1))
    (.putInt out' (aget states 0))
    (.flip out')
    (reverse-buffer! out')
    (.position in (.limit in))
    (.limit out')))

(defn- encode1* ^long [^ByteBuffer in ^objects syms ^ByteBuffer out]
  (let [raw-size (.remaining in)
        q (unsigned-bit-shift-right raw-size 2)
        i0 (- q 2)
        i1 (- (* 2 q) 2)
        i2 (- (* 3 q) 2)
        l0 (if (>= (inc i0) 0) (bit-and 0xff (.get in (inc i0))) 0)
        l1 (if (>= (inc i1) 0) (bit-and 0xff (.get in (inc i1))) 0)
        l2 (if (>= (inc i2) 0) (bit-and 0xff (.get in (inc i2))) 0)
        out' (.slice out)]
    (loop [i3 (- raw-size 2)
           l3 (bit-and 0xff (.get in (dec raw-size)))
           r3 RANS_BYTE_L]
      (if (and (> i3 (- (* 4 q) 2)) (>= i3 0))
        (let [c3 (bit-and 0xff (.get in i3))
              r3' (long (update! ^SymbolState (aget ^objects (aget syms c3) l3) out' r3))]
          (recur (dec i3) c3 r3'))
        (loop [i0 i0, i1 i1, i2 i2, i3 i3,
               l0 l0, l1 l1, l2 l2, l3 l3,
               r0 RANS_BYTE_L, r1 RANS_BYTE_L, r2 RANS_BYTE_L, r3 r3]
          (if (>= i0 0)
            (let [c0 (bit-and 0xff (.get in i0))
                  c1 (bit-and 0xff (.get in i1))
                  c2 (bit-and 0xff (.get in i2))
                  c3 (bit-and 0xff (.get in i3))
                  r3' (long (update! ^SymbolState (aget ^objects (aget syms c3) l3) out' r3))
                  r2' (long (update! ^SymbolState (aget ^objects (aget syms c2) l2) out' r2))
                  r1' (long (update! ^SymbolState (aget ^objects (aget syms c1) l1) out' r1))
                  r0' (long (update! ^SymbolState (aget ^objects (aget syms c0) l0) out' r0))]
              (recur (dec i0) (dec i1) (dec i2) (dec i3) c0 c1 c2 c3 r0' r1' r2' r3'))
            (let [r3' (long (update! ^SymbolState (aget ^objects (aget syms 0) l3) out' r3))
                  r2' (long (update! ^SymbolState (aget ^objects (aget syms 0) l2) out' r2))
                  r1' (long (update! ^SymbolState (aget ^objects (aget syms 0) l1) out' r1))
                  r0' (long (update! ^SymbolState (aget ^objects (aget syms 0) l0) out' r0))]
              (.putInt out' r3')
              (.putInt out' r2')
              (.putInt out' r1')
              (.putInt out' r0')
              (.flip out')
              (reverse-buffer! out')
              (.position in (.limit in))
              (.limit out'))))))))

(defn- allocate-output-buffer [^long raw-size]
  (let [compressed-size (+ (* 1.05 raw-size) (* 257 257 3) 9)]
    (bb/allocate-lsb-byte-buffer compressed-size)))

(defn- encode0 [^ByteBuffer in ^ByteBuffer out]
  (let [freqs (calculate-frequencies0 in)
        syms (object-array 256)
        _ (loop [i 0, total 0]
            (when (< i 256)
              (aset syms i (->SymbolState 0 0 0 0 0))
              (let [f (aget freqs i)]
                (when (> f 0)
                  (init! ^SymbolState (aget syms i) total f))
                (recur (inc i) (+ total f)))))
        out' (.slice out)
        freq-table-size (write-frequencies0 out' freqs)
        _ (.rewind in)
        compressed-data-size (encode0* in syms out')]
    (+ freq-table-size compressed-data-size)))

(defn- encode1 [^ByteBuffer in ^ByteBuffer out]
  (let [freqs (calculate-frequencies1 in)
        ^objects syms (make-array SymbolState 256 256)
        _ (dotimes [i 256]
            (let [^ints fs (aget freqs i)]
              (loop [j 0, total 0]
                (when (< j 256)
                  (aset ^objects (aget syms i) j (->SymbolState 0 0 0 0 0))
                  (let [f (aget fs j)]
                    (when (> f 0)
                      (init! ^SymbolState (aget ^objects (aget syms i) j) total f))
                    (recur (inc j) (+ total f)))))))
        out' (.slice out)
        freq-table-size (write-frequencies1 out' freqs)
        _ (.rewind in)
        compressed-data-size (encode1* in syms out')]
    (+ freq-table-size compressed-data-size)))

(def ^:private ^:const ORDER_BYTE_LEN 1)
(def ^:private ^:const COMPRESSED_BYTE_LEN 4)
(def ^:private ^:const RAW_BYTE_LEN 4)
(def ^:private ^:const PREFIX_BYTE_LEN
  (+ ORDER_BYTE_LEN COMPRESSED_BYTE_LEN RAW_BYTE_LEN))

(defn encode
  "TODO"
  ^bytes [order ^ByteBuffer in]
  (let [raw-size (.remaining in)
        out (doto ^ByteBuffer (allocate-output-buffer raw-size)
              (.position PREFIX_BYTE_LEN))
        compressed-size (case order
                          0 (encode0 in out)
                          1 (encode1 in out))]
    (.limit out (+ PREFIX_BYTE_LEN compressed-size))
    (.put out 0 (byte order))
    (.putInt out ORDER_BYTE_LEN compressed-size)
    (.putInt out (+ ORDER_BYTE_LEN COMPRESSED_BYTE_LEN) raw-size)
    (.position in 0)
    (Arrays/copyOfRange (.array out) 0 (.limit out))))

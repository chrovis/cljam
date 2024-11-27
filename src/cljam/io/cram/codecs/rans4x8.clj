(ns cljam.io.cram.codecs.rans4x8
  (:require [cljam.io.cram.itf8 :as itf8]
            [cljam.io.util.byte-buffer :as bb])
  (:import [java.nio Buffer ByteBuffer]
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

(defn- normalize-frequencies! [^ints freqs ^long total]
  (let [tr (+ (quot (bit-shift-left 4096 31) total)
              (quot (bit-shift-left 1 31) total))]
    (loop [i 0, m 0, M 0, fsum 0]
      (if (< i 256)
        (let [f (aget freqs i)]
          (if (zero? f)
            (recur (inc i) m M fsum)
            (let [f' (as-> (unsigned-bit-shift-right (* f tr) 31) f'
                       (if (zero? f') 1 f'))
                  fsum' (+ fsum f')]
              (aset freqs i f')
              (if (< m f')
                (recur (inc i) f' i fsum')
                (recur (inc i) m M fsum')))))
        (let [f (aget freqs M)
              fsum' (inc fsum)]
          (if (< fsum' 4096)
            (aset freqs M (+ f (- 4096 fsum')))
            (aset freqs M (- f (- fsum' 4096)))))))))

(defmacro ^:private ainc!
  [arr i]
  {:pre [(symbol? arr) (symbol? i)]}
  `(aset ~arr ~i (inc (aget ~arr ~i))))

(defn- calculate-frequencies0 ^ints [^ByteBuffer bb]
  (let [total (.remaining bb)
        freqs (int-array 256)]
    (dotimes [_ total]
      (let [b (long (bb/read-ubyte bb))]
        (ainc! freqs b)))
    (normalize-frequencies! freqs total)
    freqs))

(definline ^:private read-ubyte-from [^ByteBuffer bb i]
  `(bit-and 0xff (.get ~bb (long ~i))))

(defn- calculate-frequencies1 ^"[[I" [^ByteBuffer bb]
  (let [size (.remaining bb)
        ^"[[I" freqs (make-array Integer/TYPE 256 256)
        totals (int-array 256)
        _ (loop [i 0, prev 0]
            (when (< i size)
              (let [b (long (bb/read-ubyte bb))
                    ^ints fs (aget freqs prev)]
                (ainc! fs b)
                (ainc! totals prev)
                (recur (inc i) b))))
        q (unsigned-bit-shift-right size 2)
        ^ints f0 (aget freqs 0)
        b (read-ubyte-from bb q)
        _ (ainc! f0 b)
        b (read-ubyte-from bb (* 2 q))
        _ (ainc! f0 b)
        b (read-ubyte-from bb (* 3 q))
        _ (ainc! f0 b)]
    (aset totals 0 (+ (aget totals 0) 3))
    (dotimes [i 256]
      (let [total (aget totals i)]
        (when-not (zero? total)
          (normalize-frequencies! (aget freqs i) total))))
    freqs))

(defn- next-rle ^long [^long rle ^ints freqs ^long i ^ByteBuffer out]
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
    (dec rle)))

(defn- encode-itf8
  "Simplified version of ITF8 encoder for up to two bytes"
  [^ByteBuffer out ^long n]
  (if (< n 128)
    (.put out (byte n))
    (do (.put out (byte (bit-or 128 (unsigned-bit-shift-right n 8))))
        (.put out (byte (bit-and 0xff n))))))

(defn- write-frequencies0 ^long [^ByteBuffer out ^ints freqs]
  (let [start (.position out)]
    (loop [i 0, rle 0]
      (when (< i 256)
        (let [f (aget freqs i)]
          (if (zero? f)
            (recur (inc i) rle)
            (let [rle' (next-rle rle freqs i out)]
              (encode-itf8 out f)
              (recur (inc i) rle'))))))
    (.put out (byte 0))
    (- (.position out) start)))

(defn- write-frequencies1 ^long [^ByteBuffer out ^"[[I" freqs]
  (let [start (.position out)
        totals (int-array 256)
        _ (dotimes [i 256]
            (let [^ints fs (aget freqs i)]
              (dotimes [j 256]
                (aset totals i (+ (aget totals i) (aget fs j))))))]
    (loop [i 0, rle-i 0]
      (when (< i 256)
        (if (zero? (aget totals i))
          (recur (inc i) rle-i)
          (let [rle-i' (next-rle rle-i totals i out)
                ^ints fs (aget freqs i)]
            (loop [j 0, rle-j 0]
              (when (< j 256)
                (let [f (aget fs j)]
                  (if (zero? f)
                    (recur (inc j) rle-j)
                    (let [rle-j' (next-rle rle-j fs j out)]
                      (encode-itf8 out f)
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
      (set! xmax (* 0x80000 freq))
      (set! cmpl-freq (- 0x1000 freq))
      (if (< freq 2)
        (do (set! rcp-freq (bit-not 0))
            (set! rcp-shift 0)
            (set! bias (dec (+ start 0x1000))))
        (let [shift (long
                     (loop [shift 0]
                       (if (< (bit-shift-left 1 shift) freq)
                         (recur (inc shift))
                         shift)))]
          (set! rcp-freq (quot (dec (+ (bit-shift-left 0x80000000 shift) freq)) freq))
          (set! rcp-shift (dec shift))
          (set! bias start)))
      (set! rcp-shift (+ rcp-shift 32))))
  (update! [_ bb r]
    (let [x (long
             (loop [i 2, x (long r)]
               (if (or (zero? i) (< x xmax))
                 x
                 (do (.put ^ByteBuffer bb (byte (bit-and 0xff x)))
                     (recur (dec i) (unsigned-bit-shift-right x 8))))))
          q (unsigned-bit-shift-right (* x (bit-and 0xffffffff rcp-freq)) rcp-shift)]
      (+ x bias (* q cmpl-freq)))))

(def ^:private ^:const RANS_BYTE_L 0x800000)

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

(defn- encode-payload0 ^long [^ByteBuffer in ^objects syms ^ByteBuffer out]
  (let [raw-size (.remaining in)
        r (bit-and raw-size 3)
        out' (.slice out)
        r2 (if (= r 3)
             (long (update! (aget syms (read-ubyte-from in (- raw-size r -2))) out' RANS_BYTE_L))
             RANS_BYTE_L)
        r1 (if (>= r 2)
             (long (update! (aget syms (read-ubyte-from in (- raw-size r -1))) out' RANS_BYTE_L))
             RANS_BYTE_L)
        r0 (if (>= r 1)
             (long (update! (aget syms (read-ubyte-from in (- raw-size r))) out' RANS_BYTE_L))
             RANS_BYTE_L)]
    (loop [i (bit-and raw-size (bit-not 3)), r3 RANS_BYTE_L, r2 r2, r1 r1, r0 r0]
      (if (> i 0)
        (let [r3' (long (update! (aget syms (read-ubyte-from in (- i 1))) out' r3))
              r2' (long (update! (aget syms (read-ubyte-from in (- i 2))) out' r2))
              r1' (long (update! (aget syms (read-ubyte-from in (- i 3))) out' r1))
              r0' (long (update! (aget syms (read-ubyte-from in (- i 4))) out' r0))]
          (recur (- i 4) r3' r2' r1' r0'))
        (do (.putInt out' r3)
            (.putInt out' r2)
            (.putInt out' r1)
            (.putInt out' r0)
            (.flip ^Buffer out')
            (reverse-buffer! out')
            (.position ^Buffer in (.limit in))
            (.limit out'))))))

(defmacro ^:private aget2 [syms i j]
  `(aget ~(with-meta `(aget ~syms ~i) {:tag 'objects}) ~j))

(defn- encode-payload1 ^long [^ByteBuffer in ^objects syms ^ByteBuffer out]
  (let [raw-size (.remaining in)
        q (unsigned-bit-shift-right raw-size 2)
        i0 (- q 2)
        i1 (- (* 2 q) 2)
        i2 (- (* 3 q) 2)
        l0 (if (>= (inc i0) 0) (read-ubyte-from in (inc i0)) 0)
        l1 (if (>= (inc i1) 0) (read-ubyte-from in (inc i1)) 0)
        l2 (if (>= (inc i2) 0) (read-ubyte-from in (inc i2)) 0)
        out' (.slice out)]
    (loop [i3 (- raw-size 2)
           l3 (read-ubyte-from in (dec raw-size))
           r3 RANS_BYTE_L]
      (if (and (> i3 (- (* 4 q) 2)) (>= i3 0))
        (let [c3 (read-ubyte-from in i3)
              r3' (long (update! (aget2 syms c3 l3) out' r3))]
          (recur (dec i3) c3 r3'))
        (loop [i0 i0, i1 i1, i2 i2, i3 i3,
               l0 l0, l1 l1, l2 l2, l3 l3,
               r0 RANS_BYTE_L, r1 RANS_BYTE_L, r2 RANS_BYTE_L, r3 r3]
          (if (>= i0 0)
            (let [c0 (read-ubyte-from in i0)
                  c1 (read-ubyte-from in i1)
                  c2 (read-ubyte-from in i2)
                  c3 (read-ubyte-from in i3)
                  r3' (long (update! (aget2 syms c3 l3) out' r3))
                  r2' (long (update! (aget2 syms c2 l2) out' r2))
                  r1' (long (update! (aget2 syms c1 l1) out' r1))
                  r0' (long (update! (aget2 syms c0 l0) out' r0))]
              (recur (dec i0) (dec i1) (dec i2) (dec i3) c0 c1 c2 c3 r0' r1' r2' r3'))
            (let [r3' (long (update! (aget2 syms 0 l3) out' r3))
                  r2' (long (update! (aget2 syms 0 l2) out' r2))
                  r1' (long (update! (aget2 syms 0 l1) out' r1))
                  r0' (long (update! (aget2 syms 0 l0) out' r0))]
              (.putInt out' r3')
              (.putInt out' r2')
              (.putInt out' r1')
              (.putInt out' r0')
              (.flip ^Buffer out')
              (reverse-buffer! out')
              (.position ^Buffer in (.limit in))
              (.limit out'))))))))

(def ^:private ^:const PREFIX_BYTE_LEN (+ 1 4 4))

(defn- allocate-output-buffer [^long raw-size]
  ;; The size estimation code comes from:
  ;; - https://github.com/samtools/htscodecs/blob/51794289ac47455209c333182b6768f99a613948/htscodecs/rANS_static.c#L77
  ;; - https://github.com/samtools/htscodecs/blob/51794289ac47455209c333182b6768f99a613948/htscodecs/rANS_static.c#L410
  (let [allocated-size (+ (* 1.05 raw-size)
                          ;; upper bound of frequency table size
                          (* 257 257 3)
                          ;; prefix
                          PREFIX_BYTE_LEN)]
    (bb/allocate-lsb-byte-buffer allocated-size)))

(defn- encode0 ^long [^Buffer in ^ByteBuffer out]
  (let [freqs (calculate-frequencies0 in)
        syms (object-array 256)
        _ (loop [i 0, total 0]
            (when (< i 256)
              (aset syms i (->SymbolState 0 0 0 0 0))
              (let [f (aget freqs i)]
                (when (> f 0)
                  (init! (aget syms i) total f))
                (recur (inc i) (+ total f)))))
        freq-table-size (write-frequencies0 out freqs)
        _ (.rewind in)
        compressed-data-size (encode-payload0 in syms out)]
    (+ freq-table-size compressed-data-size)))

(defn- encode1 ^long [^Buffer in ^ByteBuffer out]
  (let [freqs (calculate-frequencies1 in)
        ^objects syms (make-array SymbolState 256 256)
        _ (dotimes [i 256]
            (let [^ints fs (aget freqs i)]
              (loop [j 0, total 0]
                (when (< j 256)
                  (aset ^objects (aget syms i) j (->SymbolState 0 0 0 0 0))
                  (let [f (aget fs j)]
                    (when (> f 0)
                      (init! (aget2 syms i j) total f))
                    (recur (inc j) (+ total f)))))))
        freq-table-size (write-frequencies1 out freqs)
        _ (.rewind in)
        compressed-data-size (encode-payload1 in syms out)]
    (+ freq-table-size compressed-data-size)))

(defn encode
  "Reads a byte sequence from the given ByteBuffer and encodes it by the rANS4x8 codec.
  Returns the encoded result as a byte array."
  ^bytes [^long order ^ByteBuffer in]
  (let [raw-size (.remaining in)
        ^ByteBuffer out (doto ^Buffer (allocate-output-buffer raw-size)
                          (.mark)
                          (.position PREFIX_BYTE_LEN))
        compressed-size (case order
                          0 (encode0 in out)
                          1 (encode1 in out))]
    (.reset ^Buffer out)
    (.put out (byte order))
    (.putInt out compressed-size)
    (.putInt out raw-size)
    (Arrays/copyOfRange (.array out) 0 (+ PREFIX_BYTE_LEN compressed-size))))

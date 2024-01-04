(ns cljam.io.cram.codecs.rans4x8
  (:require [cljam.io.util.byte-buffer :as bb]
            [cljam.io.cram.itf8 :as itf8]))

(defn- read-frequencies* [bb read-fn]
  (loop [sym (long (bb/read-ubyte bb))
         rle 0
         freqs (transient {})]
    (let [freqs' (assoc! freqs sym (read-fn bb))]
      (if (pos? rle)
        (recur (inc sym) (dec rle) freqs')
        (let [sym' (long (bb/read-ubyte bb))
              rle' (if (= sym' (inc sym))
                     (long (bb/read-ubyte bb))
                     rle)]
          (if (zero? sym')
            (persistent! freqs')
            (recur sym' rle' freqs')))))))

(defn- read-frequencies0 [bb]
  (read-frequencies* bb itf8/decode-itf8))

(defn- read-frequencies1 [bb]
  (read-frequencies* bb read-frequencies0))

(defn- cumulative-frequencies [freqs]
  (loop [i 0
         sum 0
         cum-freqs (transient [])]
    (if (< i 256)
      (let [f (get freqs i 0)]
        (recur (inc i) (+ sum (long f)) (conj! cum-freqs sum)))
      (persistent! (conj! cum-freqs sum)))))

(defn- lookup-symbol ^long [cum-freqs ^long f]
  (loop [l 0
         r (dec (count cum-freqs))]
    (if (< l r)
      (let [m (quot (+ l r) 2)
            fm (long (nth cum-freqs m))]
        (cond (and (<= fm f)
                   (< f (long (nth cum-freqs (inc m)))))
              m

              (< f fm) (recur l (dec m))
              :else (recur (inc m) r)))
      l)))

(defn- advance-step ^long [^long c ^long f ^long state]
  (-> (* f (bit-shift-right state 12))
      (+ (bit-and state 0xfff))
      (- c)))

(defn- renormalize-state ^long [bb ^long state]
  (loop [state state]
    (if (< state 0x800000)
      (recur (+ (bit-shift-left state 8)
                (long (bb/read-ubyte bb))))
      state)))

(defn- decode0 [bb ^long n-out]
  (let [freqs (read-frequencies0 bb)
        cum-freqs (cumulative-frequencies freqs)
        states (bb/read-ints bb 4)
        out (byte-array n-out)]
    (dotimes [i n-out]
      (let [j (rem i 4)
            state (aget states j)
            f (bit-and state 0xfff)
            sym (lookup-symbol cum-freqs f)
            state' (->> state
                        (advance-step (nth cum-freqs sym) (get freqs sym 0))
                        (renormalize-state bb))]
        (aset out i (byte sym))
        (aset states j state')))
    out))

(defn- decode1 [bb ^long n-out]
  (let [freqs (read-frequencies1 bb)
        cum-freqs (persistent!
                   (reduce-kv #(assoc! %1 %2 (cumulative-frequencies %3))
                              (transient {})
                              freqs))
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
              cfreqs (get cum-freqs last-sym)
              sym (lookup-symbol cfreqs f)
              state' (->> state
                          (advance-step (nth cfreqs sym)
                                        (get-in freqs [last-sym sym] 0))
                          (renormalize-state bb))]
          (aset out (+ i (* j quarter)) (byte sym))
          (aset states j state')
          (aset last-syms j sym))))
    (dotimes [i (- n-out truncated)]
      (let [state (aget states 3)
            f (bit-and state 0xfff)
            last-sym (aget last-syms 3)
            cfreq (get cum-freqs last-sym)
            sym (lookup-symbol cfreq f)
            state' (->> state
                        (advance-step (nth cfreq sym)
                                      (get-in freqs [last-sym sym] 0))
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

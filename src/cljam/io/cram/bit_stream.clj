(ns cljam.io.cram.bit-stream
  (:import [java.nio ByteBuffer]))

(defprotocol IBitStreamDecoder
  (read-bits [_ m]))

(definline ^:private right-nbits-of [x nbits]
  `(bit-and (long ~x) (dec (bit-shift-left 1 (long ~nbits)))))

(deftype BitStreamDecoder
         [^ByteBuffer bb
          ^:unsynchronized-mutable ^long buffer
          ^:unsynchronized-mutable ^long nbits]
  IBitStreamDecoder
  (read-bits [_ m]
    (let [m (long m)]
      (if (zero? m)
        m
        (loop [m m, buf buffer, n nbits, acc 0]
          (if (<= m n)
            (do (set! buffer buf)
                (set! nbits (- n m))
                (bit-or acc (right-nbits-of (unsigned-bit-shift-right buf nbits) m)))
            (let [m' (- m n)
                  acc' (bit-or acc (bit-shift-left (right-nbits-of buf n) m'))]
              (recur m' (long (.get bb)) 8 acc'))))))))

(defn make-bit-stream-decoder
  "Creates a new bit stream decoder based on the given byte buffer."
  [bb]
  (->BitStreamDecoder bb 0 0))

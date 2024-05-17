(ns cljam.io.cram.itf8
  (:require [cljam.io.util.byte-buffer :as bb]
            [cljam.io.util.lsb.io-stream :as lsb]))

(defn decode-itf8
  "Decodes ITF-8 integer from ByteBuffer."
  ^long [bb]
  (let [b (long (bb/read-ubyte bb))]
    (cond (zero? (bit-and b 0x80))
          b

          (zero? (bit-and b 0x40))
          (bit-or (bit-shift-left (bit-and b 0x7f) 8)
                  (long (bb/read-ubyte bb)))

          (zero? (bit-and b 0x20))
          (bit-or (bit-shift-left (bit-and b 0x3f) 16)
                  (bit-shift-left (long (bb/read-ubyte bb)) 8)
                  (long (bb/read-ubyte bb)))

          (zero? (bit-and b 0x10))
          (bit-or (bit-shift-left (bit-and b 0x1f) 24)
                  (bit-shift-left (long (bb/read-ubyte bb)) 16)
                  (bit-shift-left (long (bb/read-ubyte bb)) 8)
                  (long (bb/read-ubyte bb)))

          :else
          (unchecked-int
           (bit-or (bit-shift-left (bit-and b 0x0f) 28)
                   (bit-shift-left (long (bb/read-ubyte bb)) 20)
                   (bit-shift-left (long (bb/read-ubyte bb)) 12)
                   (bit-shift-left (long (bb/read-ubyte bb)) 4)
                   (bit-and (long (bb/read-ubyte bb)) 0x0f))))))

(defn decode-ltf8
  "Decodes LTF-8 integer from ByteBuffer."
  ^long [bb]
  (let [b (long (bb/read-ubyte bb))]
    (cond (zero? (bit-and b 0x80))
          b

          (zero? (bit-and b 0x40))
          (bit-or (bit-shift-left (bit-and b 0x7f) 8)
                  (long (bb/read-ubyte bb)))

          (zero? (bit-and b 0x20))
          (bit-or (bit-shift-left (bit-and b 0x3f) 16)
                  (bit-shift-left (long (bb/read-ubyte bb)) 8)
                  (long (bb/read-ubyte bb)))

          (zero? (bit-and b 0x10))
          (bit-or (bit-shift-left (bit-and b 0x1f) 24)
                  (bit-shift-left (long (bb/read-ubyte bb)) 16)
                  (bit-shift-left (long (bb/read-ubyte bb)) 8)
                  (long (bb/read-ubyte bb)))

          (zero? (bit-and b 0x08))
          (bit-or (bit-shift-left (bit-and b 0x07) 32)
                  (bit-shift-left (long (bb/read-ubyte bb)) 24)
                  (bit-shift-left (long (bb/read-ubyte bb)) 16)
                  (bit-shift-left (long (bb/read-ubyte bb)) 8)
                  (long (bb/read-ubyte bb)))

          (zero? (bit-and b 0x04))
          (bit-or (bit-shift-left (bit-and b 0x03) 40)
                  (bit-shift-left (long (bb/read-ubyte bb)) 32)
                  (bit-shift-left (long (bb/read-ubyte bb)) 24)
                  (bit-shift-left (long (bb/read-ubyte bb)) 16)
                  (bit-shift-left (long (bb/read-ubyte bb)) 8)
                  (long (bb/read-ubyte bb)))

          (zero? (bit-and b 0x02))
          (bit-or (bit-shift-left (bit-and b 0x01) 48)
                  (bit-shift-left (long (bb/read-ubyte bb)) 40)
                  (bit-shift-left (long (bb/read-ubyte bb)) 32)
                  (bit-shift-left (long (bb/read-ubyte bb)) 24)
                  (bit-shift-left (long (bb/read-ubyte bb)) 16)
                  (bit-shift-left (long (bb/read-ubyte bb)) 8)
                  (long (bb/read-ubyte bb)))

          (zero? (bit-and b 0x01))
          (bit-or (bit-shift-left (long (bb/read-ubyte bb)) 48)
                  (bit-shift-left (long (bb/read-ubyte bb)) 40)
                  (bit-shift-left (long (bb/read-ubyte bb)) 32)
                  (bit-shift-left (long (bb/read-ubyte bb)) 24)
                  (bit-shift-left (long (bb/read-ubyte bb)) 16)
                  (bit-shift-left (long (bb/read-ubyte bb)) 8)
                  (long (bb/read-ubyte bb)))

          :else
          (bit-or (bit-shift-left (long (bb/read-ubyte bb)) 56)
                  (bit-shift-left (long (bb/read-ubyte bb)) 48)
                  (bit-shift-left (long (bb/read-ubyte bb)) 40)
                  (bit-shift-left (long (bb/read-ubyte bb)) 32)
                  (bit-shift-left (long (bb/read-ubyte bb)) 24)
                  (bit-shift-left (long (bb/read-ubyte bb)) 16)
                  (bit-shift-left (long (bb/read-ubyte bb)) 8)
                  (long (bb/read-ubyte bb))))))

(defn encode-itf8
  "Encodes ITF-8 integer to OutputStream."
  [out ^long v]
  (cond (zero? (unsigned-bit-shift-right v 7))
        (lsb/write-ubyte out v)

        (zero? (unsigned-bit-shift-right v 14))
        (do (lsb/write-ubyte out (bit-or (unsigned-bit-shift-right v 8) 0x80))
            (lsb/write-ubyte out (bit-and v 0xff)))

        (zero? (unsigned-bit-shift-right v 21))
        (do (lsb/write-ubyte out (bit-or (unsigned-bit-shift-right v 16) 0xc0))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 8) 0xff))
            (lsb/write-ubyte out (bit-and v 0xff)))

        (zero? (unsigned-bit-shift-right v 28))
        (do (lsb/write-ubyte out (bit-or (unsigned-bit-shift-right v 24) 0xe0))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 16) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 8) 0xff))
            (lsb/write-ubyte out (bit-and v 0xff)))

        :else
        (do (lsb/write-ubyte out (bit-or (unsigned-bit-shift-right v 28) 0xf0))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 20) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 12) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 4) 0xff))
            (lsb/write-ubyte out (bit-and v 0x0f))))
  nil)

(defn encode-ltf8
  "Encodes LTF-8 integer to OutputStream."
  [out ^long v]
  (cond (zero? (unsigned-bit-shift-right v 7))
        (lsb/write-ubyte out v)

        (zero? (unsigned-bit-shift-right v 14))
        (do (lsb/write-ubyte out (bit-or (unsigned-bit-shift-right v 8) 0x80))
            (lsb/write-ubyte out (bit-and v 0xff)))

        (zero? (unsigned-bit-shift-right v 21))
        (do (lsb/write-ubyte out (bit-or (unsigned-bit-shift-right v 16) 0xc0))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 8) 0xff))
            (lsb/write-ubyte out (bit-and v 0xff)))

        (zero? (unsigned-bit-shift-right v 28))
        (do (lsb/write-ubyte out (bit-or (unsigned-bit-shift-right v 24) 0xe0))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 16) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 8) 0xff))
            (lsb/write-ubyte out (bit-and v 0xff)))

        (zero? (unsigned-bit-shift-right v 35))
        (do (lsb/write-ubyte out (bit-or (unsigned-bit-shift-right v 32) 0xf0))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 24) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 16) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 8) 0xff))
            (lsb/write-ubyte out (bit-and v 0xff)))

        (zero? (unsigned-bit-shift-right v 42))
        (do (lsb/write-ubyte out (bit-or (unsigned-bit-shift-right v 40) 0xf8))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 32) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 24) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 16) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 8) 0xff))
            (lsb/write-ubyte out (bit-and v 0xff)))

        (zero? (unsigned-bit-shift-right v 49))
        (do (lsb/write-ubyte out (bit-or (unsigned-bit-shift-right v 48) 0xfc))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 40) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 32) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 24) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 16) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 8) 0xff))
            (lsb/write-ubyte out (bit-and v 0xff)))

        (zero? (unsigned-bit-shift-right v 56))
        (do (lsb/write-ubyte out 0xfe)
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 48) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 40) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 32) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 24) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 16) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 8) 0xff))
            (lsb/write-ubyte out (bit-and v 0xff)))

        :else
        (do (lsb/write-ubyte out 0xff)
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 56) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 48) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 40) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 32) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 24) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 16) 0xff))
            (lsb/write-ubyte out (bit-and (unsigned-bit-shift-right v 8) 0xff))
            (lsb/write-ubyte out (bit-and v 0xff))))
  nil)

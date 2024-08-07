(ns cljam.io.cram.itf8-test
  (:require [cljam.io.cram.itf8 :as itf8]
            [cljam.io.util.byte-buffer :as bb]
            [clojure.test :refer [are deftest]])
  (:import [java.io ByteArrayOutputStream]))

(deftest decode-itf8-test
  (are [bs expected] (= expected
                        (-> (byte-array bs)
                            bb/make-lsb-byte-buffer
                            itf8/decode-itf8))
    [0x00] 0x00
    [0x7f] 0x7f
    [0x80 0xff] 0xff
    [0xbc 0xab] 0x3cab
    [0xc0 0x56 0x78] 0x5678
    [0xda 0xbc 0x23] 0x1abc23
    [0xe0 0x23 0x45 0x67] 0x234567
    [0xea 0xbc 0xde 0xf0] 0xabcdef0
    [0xf1 0x23 0x45 0x67 0x08] 0x12345678
    [0xff 0xff 0xff 0xff 0x0f] -1))

(deftest decode-ltf8-test
  (are [bs expected] (= expected
                        (-> (byte-array bs)
                            bb/make-lsb-byte-buffer
                            itf8/decode-ltf8))
    [0x00] 0x00
    [0x7f] 0x7f
    [0x80 0xff] 0xff
    [0xbc 0xab] 0x3cab
    [0xc0 0x56 0x78] 0x5678
    [0xda 0xbc 0x23] 0x1abc23
    [0xe0 0x23 0x45 0x67] 0x234567
    [0xea 0xbc 0xde 0xf0] 0xabcdef0
    [0xf0 0xde 0xad 0xbe 0xef] 0xdeadbeef
    [0xf7 0xff 0xff 0xff 0xff] 0x7ffffffff
    [0xf8 0x12 0x34 0x56 0x78 0x9a] 0x123456789a
    [0xfb 0xff 0xff 0xff 0xff 0xff] 0x3ffffffffff
    [0xfc 0xba 0xdb 0xad 0xba 0xdb 0xad] 0xbadbadbadbad
    [0xfd 0x23 0x45 0x67 0x89 0xab 0xcd] 0x123456789abcd
    [0xfe 0x12 0x34 0x56 0x78 0x9a 0xbc 0xde] 0x123456789abcde
    [0xfe 0xff 0xff 0xff 0xff 0xff 0xff 0xff] 0xffffffffffffff
    [0xff 0x12 0x34 0x56 0x78 0x9a 0xbc 0xde 0xf0] 0x123456789abcdef0
    [0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff] -1))

(deftest encode-itf8-test
  (are [v expected] (= expected
                       (let [baos (ByteArrayOutputStream.)]
                         (itf8/encode-itf8 baos v)
                         (map #(bit-and % 0xff) (.toByteArray baos))))
    0 [0x00]
    0x7f [0x7f]
    0xff [0x80 0xff]
    0x3cab [0xbc 0xab]
    0x5678 [0xc0 0x56 0x78]
    0x1abc23 [0xda 0xbc 0x23]
    0x234567 [0xe0 0x23 0x45 0x67]
    0xabcdef0 [0xea 0xbc 0xde 0xf0]
    0x12345678 [0xf1 0x23 0x45 0x67 0x08]
    -1 [0xff 0xff 0xff 0xff 0x0f]))

(deftest encode-ltf8-test
  (are [v expected] (= expected
                       (let [baos (ByteArrayOutputStream.)]
                         (itf8/encode-ltf8 baos v)
                         (map #(bit-and % 0xff) (.toByteArray baos))))
    0 [0x00]
    0x7f [0x7f]
    0xff [0x80 0xff]
    0x3cab [0xbc 0xab]
    0x5678 [0xc0 0x56 0x78]
    0x1abc23 [0xda 0xbc 0x23]
    0x234567 [0xe0 0x23 0x45 0x67]
    0xabcdef0 [0xea 0xbc 0xde 0xf0]
    0xdeadbeef [0xf0 0xde 0xad 0xbe 0xef]
    0x7ffffffff [0xf7 0xff 0xff 0xff 0xff]
    0x123456789a [0xf8 0x12 0x34 0x56 0x78 0x9a]
    0x3ffffffffff [0xfb 0xff 0xff 0xff 0xff 0xff]
    0xbadbadbadbad [0xfc 0xba 0xdb 0xad 0xba 0xdb 0xad]
    0x123456789abcd [0xfd 0x23 0x45 0x67 0x89 0xab 0xcd]
    0x123456789abcde [0xfe 0x12 0x34 0x56 0x78 0x9a 0xbc 0xde]
    0xffffffffffffff [0xfe 0xff 0xff 0xff 0xff 0xff 0xff 0xff]
    0x123456789abcdef0 [0xff 0x12 0x34 0x56 0x78 0x9a 0xbc 0xde 0xf0]
    -1 [0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff]))

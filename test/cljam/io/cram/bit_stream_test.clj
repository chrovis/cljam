(ns cljam.io.cram.bit-stream-test
  (:require [cljam.io.cram.bit-stream :as bs]
            [cljam.io.util.byte-buffer :as bb]
            [clojure.test :refer [deftest is]]))

(deftest read-bits-test
  (let [bb (bb/make-lsb-byte-buffer (byte-array [0x31 0x41 0x59 0x26]))
        bs-decoder (bs/make-bit-stream-decoder bb)]
    (is (= [3 1 4 1 5 9 2 6] (repeatedly 8 #(bs/read-bits bs-decoder 4)))))
  (let [bb (bb/make-lsb-byte-buffer (byte-array [2r11001110 2r10101110]))
        bs-decoder (bs/make-bit-stream-decoder bb)]
    (is (= [6 3 5 2 7] (repeatedly 5 #(bs/read-bits bs-decoder 3))))))

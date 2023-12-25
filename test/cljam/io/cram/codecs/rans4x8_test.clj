(ns cljam.io.cram.codecs.rans4x8-test
  (:require [cljam.io.cram.codecs.rans4x8 :as rans]
            [cljam.io.util.byte-buffer :as bb]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.io FileInputStream]
           [java.nio ByteBuffer]
           [java.util Arrays]))

(deftest read-frequencies0-test
  ;; cited from https://github.com/samtools/hts-specs/blob/346a94a9980b0105e926a019b4e62fa1b9e30910/CRAMcodecs.tex#L389-L394
  (let [arr (byte-array [0x61      0x87 0x47
                         0x62 0x02 0x82 0xe8
                         #_>       0x81 0x74
                         #_>       0x81 0x74
                         0x72      0x82 0xe8
                         0x00])
        bb (bb/make-lsb-byte-buffer arr)]
    (is (= {0x61 0x747
            0x62 0x2e8
            0x63 0x174
            0x64 0x174
            0x72 0x2e8}
           (#'rans/read-frequencies0 bb)))
    (is (zero? (.remaining bb)))))

(deftest read-frequencies1-test
  ;; cited from https://github.com/samtools/hts-specs/blob/346a94a9980b0105e926a019b4e62fa1b9e30910/CRAMcodecs.tex#L489-L516
  (let [arr (byte-array [0x00
                         0x61      0x8f 0xff
                         0x00

                         0x61
                         0x61      0x82 0x86
                         0x62 0x02 0x86 0xbd
                         #_>       0x83 0x5e
                         #_>       0x83 0x5e
                         0x00

                         0x62 0x02
                         0x72      0x8f 0xff
                         0x00

                         0x61      0x8f 0xff
                         0x00

                         0x61      0x8f 0xff
                         0x00

                         0x72
                         0x61      0x8f 0xff
                         0x00

                         0x00])
        bb (bb/make-lsb-byte-buffer arr)]
    (is (= {0x00 {0x61 0xfff}
            0x61 {0x61 0x286
                  0x62 0x6bd
                  0x63 0x35e
                  0x64 0x35e}
            0x62 {0x72 0xfff}
            0x63 {0x61 0xfff}
            0x64 {0x61 0xfff}
            0x72 {0x61 0xfff}}
           (#'rans/read-frequencies1 bb)))
    (is (zero? (.remaining bb)))))

(defn- read-as-buffer ^ByteBuffer [file]
  (with-open [in (FileInputStream. (io/file file))]
    (let [ch (.getChannel in)
          bb (bb/allocate-lsb-byte-buffer (.size ch))]
      (.read ch bb)
      (.flip bb)
      bb)))

(deftest decode-test
  (testing "Order-0"
    (let [uncompressed (read-as-buffer "test-resources/cram/codecs/rans4x8/uncompressed_order0.dat")
          compressed (read-as-buffer "test-resources/cram/codecs/rans4x8/compressed_order0.dat.rans4x8")]
      (is (Arrays/equals ^bytes (.array uncompressed) ^bytes (rans/decode compressed)))
      (is (zero? (.remaining compressed)))))
  (testing "Order-1"
    (let [uncompressed (read-as-buffer "test-resources/cram/codecs/rans4x8/uncompressed_order1.dat")
          compressed (read-as-buffer "test-resources/cram/codecs/rans4x8/compressed_order1.dat.rans4x8")]
      (is (Arrays/equals ^bytes (.array uncompressed) ^bytes (rans/decode compressed)))
      (is (zero? (.remaining compressed))))))

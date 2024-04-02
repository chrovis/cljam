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
           (into {} (keep-indexed (fn [i x] (when (pos? x) [i x])))
                 (#'rans/read-frequencies0 bb))))
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
           (into {}
                 (keep-indexed
                  (fn [i arr]
                    (when-let [m (->> arr
                                      (into {}
                                            (keep-indexed
                                             (fn [j x]
                                               (when (pos? x)
                                                 [j x]))))
                                      not-empty)]
                      [i m])))
                 (#'rans/read-frequencies1 bb))))
    (is (zero? (.remaining bb)))))

(deftest cumulative-frequencies-test
  (let [freqs (-> [0x000 0x747 0x000 0x2e8 0x000 0x174 0x174 0x2e8]
                  (concat (repeat 248 0x000))
                  int-array)]
    (is (= (concat [0x000 0x000 0x747 0x747 0xa2f 0xa2f 0xba3 0xd17 0xfff 0xfff]
                   (repeat 246 0xfff))
           (vec (#'rans/cumulative-frequencies freqs))))))

(deftest reverse-lookup-table-test
  (let [freqs (-> [0x000 0x747 0x000 0x2e8 0x000 0x174 0x174 0x2e8]
                  (concat (repeat 248 0x000))
                  int-array)
        cfreqs (#'rans/cumulative-frequencies freqs)
        table ^bytes (#'rans/reverse-lookup-table cfreqs)]
    (is (= 1 (aget table 0x000)))
    (is (= 1 (aget table 0x746)))
    (is (= 3 (aget table 0x747)))
    (is (= 3 (aget table 0xa2e)))
    (is (= 5 (aget table 0xa2f)))
    (is (= 5 (aget table 0xba2)))
    (is (= 6 (aget table 0xba3)))
    (is (= 6 (aget table 0xd16)))
    (is (= 7 (aget table 0xd17)))
    (is (= 7 (aget table 0xffe)))
    (is (= 255 (bit-and (aget table 0xfff) 0xff)))))

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

(ns cljam.io.cram.codecs.rans4x8-test
  (:require [cljam.io.cram.codecs.rans4x8 :as rans]
            [cljam.io.util.byte-buffer :as bb]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.io FileInputStream]
           [java.nio ByteBuffer]
           [java.util Arrays]))

(defn- arr->freq-map [arr]
  (into {} (keep-indexed (fn [i x] (when (pos? x) [i x]))) arr))

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
           (arr->freq-map (#'rans/read-frequencies0 bb))))
    (is (zero? (.remaining bb)))))

(defn- arr->freq-map2 [arr]
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
        arr))

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
           (arr->freq-map2 (#'rans/read-frequencies1 bb))))
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

(deftest calculate-frequencies0-test
  (let [arr (.getBytes "abracadabra")
        bb (bb/make-lsb-byte-buffer arr)]
    (is (= {0x61 0x747
            0x62 0x2e8
            0x63 0x174
            0x64 0x174
            0x72 0x2e8}
           (arr->freq-map (#'rans/calculate-frequencies0 bb)))))
  (let [arr (->> (concat (repeat 6144 1)
                         (repeat 2048 2)
                         (repeat    1 3))
                 shuffle
                 byte-array)
        bb (bb/make-lsb-byte-buffer arr)]
    (is (= {1 3070
            2 1024
            3 1}
           (arr->freq-map (#'rans/calculate-frequencies0 bb))))))

(deftest calculate-frequencies1-test
  (let [arr (.getBytes "abracadabraabracadabraabracadabraabracadabr")
        bb (bb/make-lsb-byte-buffer arr)]
    (is (= {0x00 {0x61 0x7ff
                  0x62 0x400
                  0x72 0x400}
            0x61 {0x61 0x286
                  0x62 0x6bd
                  0x63 0x35e
                  0x64 0x35e}
            0x62 {0x72 0xfff}
            0x63 {0x61 0xfff}
            0x64 {0x61 0xfff}
            0x72 {0x61 0xfff}}
           (arr->freq-map2 (#'rans/calculate-frequencies1 bb)))))
  (let [arr (->> (concat (repeat 6144 1)
                         (repeat 2048 2)
                         (repeat    1 3))
                 shuffle
                 (interleave (repeat 100))
                 byte-array)
        bb (bb/make-lsb-byte-buffer arr)]
    (is (= {100 {1 3070
                 2 1024
                 3 1}
            1 {100 4095}
            2 {100 4095}
            3 {100 4095}}
           (-> (arr->freq-map2 (#'rans/calculate-frequencies1 bb))
               (dissoc 0))))))

(deftest write-frequencies0-test
  (let [freqs {0x61 1863
               0x62 744
               0x63 372
               0x64 372
               0x72 744}
        arr (int-array 256)
        _ (doseq [[s f] freqs]
            (aset arr s f))
        bb (#'rans/allocate-output-buffer 11)]
    (#'rans/write-frequencies0 bb arr)
    (.flip bb)
    (is (= freqs (arr->freq-map (#'rans/read-frequencies0 bb))))))

(deftest write-frequencies1-test
  (let [freqs {0x00 {0x61 0x7ff
                     0x62 0x400
                     0x72 0x400}
               0x61 {0x61 0x286
                     0x62 0x6bd
                     0x63 0x35e
                     0x64 0x35e}
               0x62 {0x72 0xfff}
               0x63 {0x61 0xfff}
               0x64 {0x61 0xfff}
               0x72 {0x61 0xfff}}
        ^"[[I" arr (make-array Integer/TYPE 256 256)
        _ (doseq [[s1 fs] freqs
                  [s2 f] fs]
            (aset ^ints (aget arr s1) s2 f))
        bb (#'rans/allocate-output-buffer 43)]
    (#'rans/write-frequencies1 bb arr)
    (.flip bb)
    (is (= freqs (arr->freq-map2 (#'rans/read-frequencies1 bb))))))

(deftest encode-test
  (testing "Order-0"
    (let [uncompressed (read-as-buffer "test-resources/cram/codecs/rans4x8/uncompressed_order0.dat")
          compressed (bb/make-lsb-byte-buffer (rans/encode 0 uncompressed))]
      (is (Arrays/equals (.array uncompressed) (rans/decode compressed)))))
  (testing "Order-1"
    (let [uncompressed (read-as-buffer "test-resources/cram/codecs/rans4x8/uncompressed_order1.dat")
          compressed (bb/make-lsb-byte-buffer (rans/encode 1 uncompressed))]
      (is (Arrays/equals (.array uncompressed) (rans/decode compressed))))))

(ns cljam.bcf.t-writer
  (:require [clojure.test :refer :all]
            [cljam.bcf.writer :as bcf-writer])
  (:import [java.nio ByteBuffer]))

(deftest about-encode-variant-shared
  (let [bb (#'bcf-writer/encode-variant-shared
            {:chr 0, :pos 1, :ref "A", :ref-length 1, :qual 1.0, :alt ["C"], :info {0 1 10 300},
             :format [0 1], :id "TEST", :filter [0], :genotype {0 [0 1] 1 [16 32]}, :n-sample 2})]
    (is
     (= (seq (.array ^ByteBuffer bb))
        [0 0 0 0 ;; uint32 chrom
         0 0 0 0 ;; uint32 pos-1
         1 0 0 0 ;; uint32 ref-length
         0 0 (unchecked-byte 0x80) 0x3f ;; float32 qual
         2 0 2 0 ;; uint32 n-allele << 16 | n-info
         2 0 0 2 ;; uint32 n-fmt << 24 | n-sample = 2 << 24 | 2
         0x47 84 69 83 84 ;; 4 x 7(char), T, E, S, T
         0x17 65 ;; 1 x 7(char), A ref
         0x17 67 ;; 1 x 7(char), C alt
         0x11 0  ;; 1 x 1(uint8), 0 filter
         0x11 0  ;; 1 x 1(uint8), 0 info-key1
         0x11 1  ;; 1 x 1(uint8), 1 info-value1
         0x11 10 ;; 1 x 1(uint8), 10 info-key2
         0x12 0x2c 0x01 ;; 1 x 2(uint16), 300 inf0-value2
         ]))))

(deftest about-encode-variant-indv
  (let [bb (#'bcf-writer/encode-variant-indv
            {:genotype {0 [0 1] 1 [16 32] :n-sample 2}})]
    (is
     (= (seq (.array ^ByteBuffer bb))
        [0x11 0
         0x11 0 1
         0x11 1
         0x11 16 32]))))

(deftest about-encode-variant-indv
  (let [bb (#'bcf-writer/encode-variant-indv
            {:genotype {0 [[2 3] [4 3] [4 4]] 1 [48 48 43] 2 [1 8 5] 3 [[51 51] [51 51] [nil nil]]}
             :n-sample 3})]
    (is
     (= (seq (.array ^ByteBuffer bb))
        [0x11 0
         0x21 2 3 4 3 4 4
         0x11 1
         0x11 48 48 43
         0x11 2
         0x11 1 8 5
         0x11 3
         0x21 51 51 51 51 (unchecked-byte 0x80) (unchecked-byte 0x80)]))))

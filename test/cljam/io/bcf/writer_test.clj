(ns cljam.io.bcf.writer-test
  (:require [clojure.test :refer [deftest is are]]
            [clojure.java.io :as cio]
            [clojure.string :as cstr]
            [cljam.io.bcf.writer :as bcf-writer]
            [cljam.io.util.bgzf :as bgzf]
            [cljam.test-common :refer
             [with-before-after
              prepare-cache!
              clean-cache!
              temp-dir]])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.io ByteArrayOutputStream File]))

(deftest about-encode-typed-value
  (let [ba (#'bcf-writer/encode-typed-value :int (vec (repeat 14 1)))]
    (is
     (= (seq ba)
        [(unchecked-byte 0xE1) 1 1 1 1 1 1 1 1 1 1 1 1 1 1])))
  (let [ba (#'bcf-writer/encode-typed-value :int (vec (repeat 15 1)))]
    (is
     (= (seq ba)
        [(unchecked-byte 0xF1) 0x11 0x0F 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1])))
  (let [ba (#'bcf-writer/encode-typed-value :int (vec (repeat 127 1)))]
    (is
     (= (seq ba)
        (concat (map unchecked-byte [0xF1 0x11 0x7F]) (repeat 127 1)))))
  (let [ba (#'bcf-writer/encode-typed-value :int (vec (repeat 255 1)))]
    (is
     (= (seq ba)
        (concat (map unchecked-byte [0xF1 0x12 0xFF 0x00]) (repeat 255 1)))))
  (let [ba (#'bcf-writer/encode-typed-value :int (vec (repeat 32768 1)))]
    (is
     (= (seq ba)
        (concat (map unchecked-byte [0xF1 0x13 0x00 0x80 0x00 0x00])
                (repeat 32768 1))))))

(defn- bb->seq [^ByteBuffer bb]
  (seq (.array bb)))

(defn- bytes->str [xs]
  (cstr/join \space (map #(format "0x%02x" %) xs)))

(deftest about-encode-variant-shared
  (are [?var ?bytes]
       (= (map unchecked-byte ?bytes)
          (bb->seq (@#'bcf-writer/encode-variant-shared ?var)))

    {:chr 0, :pos 1, :ref "N", :ref-length 1, :qual nil,
     :alt nil, :info [], :format nil, :id nil, :filter nil,
     :genotype nil, :n-sample 0}
    [0x00 0x00 0x00 0x00 ;; int32 chrom
     0x00 0x00 0x00 0x00 ;; int32 pos-1
     0x01 0x00 0x00 0x00 ;; uint32 ref-length
     0x01 0x00 0x80 0x7f ;; float32 qual = nil
     0x00 0x00 0x01 0x00 ;; uint32 n-allele << 16 | n-info
     0x00 0x00 0x00 0x00 ;; uint32 n-fmt << 24 | n-sample
     0x07 ;; 0 x 7(char) id = nil
     0x17 0x4e ;; 1 x 7(char) N ref
     0x00] ;; 0 filter

    {:chr 0, :pos 1, :ref "A", :ref-length 1, :qual 1.0, :alt ["C"],
     :info [[0 :int 1] [10 :int 300]], :format [0 1], :id "TEST", :filter [0],
     :genotype [[0 :int [0 1]] [1 :int [16 32]]], :n-sample 2}
    [0x00 0x00 0x00 0x00 ;; int32 chrom
     0x00 0x00 0x00 0x00 ;; int32 pos-1
     0x01 0x00 0x00 0x00 ;; uint32 ref-length
     0x00 0x00 0x80 0x3f ;; float32 qual
     0x02 0x00 0x02 0x00 ;; uint32 n-allele << 16 | n-info
     0x02 0x00 0x00 0x02 ;; uint32 n-fmt << 24 | n-sample
     0x47 0x54 0x45 0x53 0x54 ;; 4 x 7(char), T, E, S, T
     0x17 0x41 ;; 1 x 7(char), A ref
     0x17 0x43 ;; 1 x 7(char), C alt
     0x11 0x00  ;; 1 x 1(uint8), 0 filter PASS
     0x11 0x00  ;; 1 x 1(uint8), 0 info-key1
     0x11 0x01  ;; 1 x 1(uint8), 1 info-value1
     0x11 0x0a ;; 1 x 1(uint8), 10 info-key2
     0x12 0x2c 0x01] ;; 1 x 2(uint16), 300 info-value2

    {:chr 2, :pos 35, :ref "AC", :ref-length 2, :qual nil,
     :alt ["ACCC" "ACCCC"],
     :info [[1 :str "VariantCallFormatSampleText"]
            [2 :str ["FOO" "BAR"]]
            [4 :int [256]]],
     :format [], :id nil, :filter [0], :genotype {}, :n-sample 0}
    [0x02 0x00 0x00 0x00 ;; int32 chrom
     0x22 0x00 0x00 0x00 ;; int32 pos-1
     0x02 0x00 0x00 0x00 ;; uint32 ref-length
     0x01 0x00 0x80 0x7f ;; float32 qual = nil
     0x03 0x00 0x03 0x00 ;; uint32 n-allele << 16 | n-info
     0x00 0x00 0x00 0x00 ;; uint32 n-fmt << 24 | n-sample
     0x07 ;; 0 x 7(char) id = nil
     0x27 0x41 0x43 ;; 2 x 7(char), AC ref
     0x47 0x41 0x43 0x43 0x43 ;; 4 x 7(char) ACCC alt
     0x57 0x41 0x43 0x43 0x43 0x43 ;; 5 x 7(char) ACCCC alt
     0x11 0x00 ;; 1 x 1(uint8), 0 filter PASS
     0x11 0x01 ;; 1 x 1(uint8), 1 info-key
     0xf7 0x11 0x1b ;; 0xF0 (size follows) | 7(char), 1 x 1(uint8), 27 (length)
     0x56 0x61 0x72 0x69 0x61 0x6e 0x74 ;; Variant
     0x43 0x61 0x6c 0x6c ;; Call
     0x46 0x6f 0x72 0x6d 0x61 0x74 ;; Format
     0x53 0x61 0x6d 0x70 0x6c 0x65 ;; Sample
     0x54 0x65 0x78 0x74 ;; Text
     0x11 0x02 ;; 1 x 1(uint8), 2 info-key
     0x77 0x46 0x4f 0x4f 0x2c 0x42 0x41 0x52
     0x11 0x04 ;; 1 x 1(uint8), 4 info-key
     0x12 0x00 0x01] ;; 1 x 2(uint16) 0x100

    {:chr 2, :pos 321682, :id "bnd_V", :ref "T", :alt ["]13:123456]T"],
     :ref-length 1, :qual 6.0, :filter [0],
     :info [[1 :char [\a \b \c]]],
     :format [], :genotype {}, :n-sample 0}
    [0x02 0x00 0x00 0x00 ;; int32 chrom
     0x91 0xe8 0x04 0x00 ;; int32 pos-1
     0x01 0x00 0x00 0x00 ;; uint32 ref-length
     0x00 0x00 0xc0 0x40 ;; float32 qual
     0x01 0x00 0x02 0x00 ;; uint32 n-allele << 16 | n-info
     0x00 0x00 0x00 0x00 ;; uint32 n-fmt << 24 | n-sample
     0x57 0x62 0x6e 0x64 0x5f 0x56 ;; 5 x 7(char), id = bnd_V
     0x17 0x54 ;; 1 x 7(char), ref=T
     ;; 12 x 7(char) ]13:123456]T
     0xc7 0x5d 0x31 0x33 0x3a 0x31 0x32 0x33 0x34 0x35 0x36 0x5d 0x54
     0x11 0x00 ;; 1 x 1(uint8), 0 filter PASS
     0x11 0x01 ;; 1 x 1(uint8), 1 info-key
     0x57 0x61 0x2c 0x62 0x2c 0x63] ;; 5 x 7(char), a,b,c
    ))

(deftest about-encode-variant-indv
  (are [?var ?bytes]
       (= (bytes->str (map unchecked-byte ?bytes))
          (bytes->str (bb->seq (#'bcf-writer/encode-variant-indv ?var))))
    {:genotype [[0 :int [[0] [1]]]
                [1 :int [[16] [32]]]],
     :n-sample 2}
    [0x11 0x00 ;; 1 x 1(uint8), 0 genotype key
     0x11 0x00 0x01 ;; 1 x 1(uint8), [0 1]
     0x11 0x01 ;; 1 x 1(uint8), 1 genotype key
     0x11 0x10 0x20] ;; 1 x 1(uint8), [16 32]

    {:genotype [[0 :int [[2 3] [4 3] [4 4]]]
                [1 :int [[48] [48] [43]]]
                [2 :int [[1] [8] [5]]]
                [3 :int [[51 51] [51 51] [nil nil]]]],
     :n-sample 3}
    [0x11 0x00
     0x21 0x02 0x03 0x04 0x03 0x04 0x04
     0x11 0x01
     0x11 0x30 0x30 0x2b
     0x11 0x02
     0x11 0x01 0x08 0x05
     0x11 0x03
     0x21 0x33 0x33 0x33 0x33 0x80 0x80]

    {:genotype [[1 :str [["AA" "BB" "CC"] ["D" "E" "F"]]]
                [2 :char [[\a \b \c] [\d]]]],
     :n-sample 2}
    [0x11 0x01 ;; 1 x 1(uint8), 1 genotype key
     0x87 ;; 8 x 7(char)
     0x41 0x41 0x2c 0x42 0x42 0x2c 0x43 0x43 ;; AA,BB,CC
     0x44 0x2c 0x45 0x2c 0x46 0x00 0x00 0x00 ;; D,E,F\0\0\0
     0x11 0x02 ;; 1 x 1(uint8), 2 genotype key
     0x57 ;; 6 x 7(char)
     0x61 0x2c 0x62 0x2c 0x63  ;; a,b,c
     0x64 0x00 0x00 0x00 0x00] ;; d\0\0\0\0

    {:genotype [[1 :str [nil ["ALT1" "ALT2"] nil]]],
     :n-sample 3}
    [0x11 0x01
     0x97
     0x2e 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
     0x41 0x4c 0x54 0x31 0x2c 0x41 0x4c 0x54 0x32
     0x2e 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00]

    {:genotype [[2 :int [[1] [2 3] nil]]],
     :n-sample 3}
    [0x11 0x02
     0x21
     0x01 0x81
     0x02 0x03
     0x80 0x81]

    {:genotype [[3 :int [[1] [2 3 4] nil]]],
     :n-sample 3}
    [0x11 0x03
     0x31
     0x01 0x81 0x81
     0x02 0x03 0x04
     0x80 0x81 0x81]

    {:genotype [[4 :int [[1 nil nil] [2 3 4] nil]]],
     :n-sample 3}
    [0x11 0x04
     0x31
     0x01 0x80 0x80
     0x02 0x03 0x04
     0x80 0x81 0x81]

    {:genotype [[5 :int [1 2 nil]]],
     :n-sample 3}
    [0x11 0x05
     0x11
     0x01
     0x02
     0x80]

    {:genotype [[6 :float [[1.0] [2.0 3.0] nil]]],
     :n-sample 3}
    [0x11 0x06
     0x25
     0x00 0x00 0x80 0x3f 0x02 0x00 0x80 0x7f
     0x00 0x00 0x00 0x40 0x00 0x00 0x40 0x40
     0x01 0x00 0x80 0x7f 0x02 0x00 0x80 0x7f]

    {:genotype [[7 :int [nil [2 2] [2 4]]]],
     :n-sample 3}
    [0x11 0x07
     0x21
     0x80 0x81
     0x02 0x02
     0x02 0x04]))

(defn- bgzf->bb ^ByteBuffer [f]
  (with-open [is (bgzf/bgzf-input-stream f)
              baos (ByteArrayOutputStream.)]
    (while (pos? (.available is))
      (.write baos (.read is)))
    (.flush baos)
    (doto (ByteBuffer/wrap (.toByteArray baos))
      (.order ByteOrder/LITTLE_ENDIAN))))

(defn- read-variant-blocks! [f]
  (let [bb (bgzf->bb f)]
    (doseq [b (map byte [\B \C \F 2 2])]
      (assert (= b (.get bb))))
    (.position bb (+ (.getInt bb) (.position bb)))
    (->> (repeatedly
          #(when (.hasRemaining bb)
             (let [shared (byte-array (.getInt bb))
                   indiv (byte-array (.getInt bb))]
               (.get bb shared)
               (.get bb indiv)
               [(vec shared) (vec indiv)])))
         (take-while some?)
         vec)))

(defn- write-variants&read-blocks [meta-info header variants]
  (with-before-after {:before (prepare-cache!)
                      :after (clean-cache!)}
    (let [tmp (cio/file temp-dir "write-variants.bcf")]
      (with-open [w (bcf-writer/writer tmp meta-info header)]
        (bcf-writer/write-variants w variants))
      (read-variant-blocks! tmp))))

(deftest write-variants
  (let [meta-info {:fileformat "VCFv4.3"
                   :contig [{:id "1", :length 100}]}
        header ["CHROM" "POS" "ID" "REF" "ALT" "QUAL" "FILTER" "INFO"
                "FORMAT" "SAMPLE"]]
    (are [?variants ?blocks]
         (= (map #(map (partial map unchecked-byte) %) ?blocks)
            (write-variants&read-blocks meta-info header ?variants))
      [{:chr "1", :pos 10, :ref "A", :alt ["C"],
        :info {:UNDECLARED-INFO #{"???"}},
        :FORMAT [:UNDECLARED-FORMAT]}]
      [[[0x00 0x00 0x00 0x00
         0x09 0x00 0x00 0x00
         0x01 0x00 0x00 0x00
         0x01 0x00 0x80 0x7f
         0x00 0x00 0x02 0x00
         0x01 0x00 0x00 0x00
         0x07
         0x17 0x41
         0x17 0x43
         0x00]
        []]])))

(deftest default-meta-info
  (let [tmp (File/createTempFile "default-meta-info" ".bcf")
        header ["CHROM" "POS" "ID" "REF" "ALT" "QUAL" "FILTER" "INFO"]
        s (->> ["##fileformat=VCFv4.3"
                "##FILTER=<ID=PASS,Description=\"All filters passed\",IDX=0>"
                (str "#" (cstr/join \tab header) \newline (char 0))]
               (cstr/join \newline))
        _ (with-open [_ (bcf-writer/writer tmp {} header)])
        bytes (bb->seq (bgzf->bb tmp))]
    (is (= (concat (.getBytes "BCF\2\2") [(.length s) 0 0 0] (.getBytes s))
           bytes))
    (.delete tmp)))

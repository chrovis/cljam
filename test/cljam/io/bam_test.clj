(ns cljam.io.bam-test
  "Tests for Processing BAM files."
  (:require [cljam.io.bam.decoder :as decoder]
            [cljam.io.bam.encoder :as encoder]
            [cljam.io.util.byte-buffer :as bb]
            [clojure.test :refer [are deftest testing]])
  (:import [java.io ByteArrayOutputStream]))

(deftest aux-data-codec-test
  (testing "non-array types"
    (are [?type ?value]
         (= ?value
            (let [baos (ByteArrayOutputStream.)]
              (#'encoder/encode-tag-value baos ?type ?value)
              (decoder/parse-tag-single ?type (bb/make-lsb-byte-buffer (.toByteArray baos)))))
      \A \@
      \A \A
      \A \z
      \c -128
      \c -1
      \c 0
      \c 127
      \C 0
      \C 127
      \C 128
      \C 255
      \s -32768
      \s -1
      \s 0
      \s 32767
      \S 0
      \S 32767
      \S 32768
      \S 65535
      \i -2147483648
      \i 0
      \i -1
      \i 2147483647
      \I 0
      \I 2147483647
      \I 2147483648
      \I 4294967295
      \f -17.75
      \f 0.0
      \f 5.75
      \f Float/MAX_VALUE
      \f Float/MIN_NORMAL
      \f Float/MIN_VALUE
      \f Float/NEGATIVE_INFINITY
      \f Float/POSITIVE_INFINITY))

  (testing "NULL-terminated text"
    (are [?type ?value]
         (= ?value
            (let [baos (ByteArrayOutputStream.)]
              (#'encoder/encode-tag-value baos ?type ?value)
              (decoder/parse-tag-single ?type (bb/make-lsb-byte-buffer (.toByteArray baos)))))
      \Z "aaaBBB0011223344@@@+++"
      \Z (str "!\"#$%&'()*+,-./0123456789:;<=>?@"
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")))

  (testing "array types"
    (are [?value]
         (= ?value
            (let [baos (ByteArrayOutputStream.)]
              (#'encoder/encode-tag-value baos \B ?value)
              (#'decoder/parse-tag-array (bb/make-lsb-byte-buffer (.toByteArray baos)))))
      "c,0"
      "C,0"
      "s,0"
      "S,0"
      "i,0"
      "I,0"
      "f,0.0"
      "c,-128,0,127"
      "C,0,255"
      "s,-32768,0,32767"
      "S,0,65535"
      "i,-2147483648,0,2147483647"
      "I,0,4294967295"
      "f,-17.25,0.0,5.75"
      (str "f,3.4028235E38,1.4E-45,-Infinity,Infinity," Float/MIN_NORMAL))))

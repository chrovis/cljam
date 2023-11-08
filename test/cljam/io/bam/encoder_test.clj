(ns cljam.io.bam.encoder-test
  "Tests for cljam.io.bam.encoder."
  (:require [clojure.test :refer [deftest are testing]]
            [clojure.string :as cstr]
            [cljam.io.bam.encoder :as encoder]
            [cljam.test-common :as test-common])
  (:import [java.io ByteArrayOutputStream DataOutputStream]))

(defn- get-encoded-option-data [?type ?values]
  (let [baos (ByteArrayOutputStream.)]
    (#'encoder/encode-tag-value baos ?type ?values)
    (seq (.toByteArray baos))))

(deftest encode-tag-value-test
  (testing "non-array types"
    (are [?type ?values ?bytes]
         (= (map unchecked-byte ?bytes)
            (get-encoded-option-data ?type ?values))
      \A \@ [0x40]
      \A \A [0x41]
      \A \z [0x7a]
      \c 0 [0x00]
      \c 127 [0x7f]
      \c -128 [0x80]
      \c -1 [0xff]
      \C 0 [0x00]
      \C 127 [0x7f]
      \C 128 [0x80]
      \C 255 [0xff]
      \s 0 [0x00 0x00]
      \s 32767 [0xff 0x7f]
      \s -32768 [0x00 0x80]
      \s -1 [0xff 0xff]
      \S 0 [0x00 0x00]
      \S 32767 [0xff 0x7f]
      \S 32768 [0x00 0x80]
      \S 65535 [0xff 0xff]
      \i 0 [0x00 0x00 0x00 0x00]
      \i 2147483647 [0xff 0xff 0xff 0x7f]
      \i -2147483648 [0x00 0x00 0x00 0x80]
      \i -1 [0xff 0xff 0xff 0xff]
      \I 0 [0x00 0x00 0x00 0x00]
      \I 2147483647 [0xff 0xff 0xff 0x7f]
      \I 2147483648 [0x00 0x00 0x00 0x80]
      \I 4294967295 [0xff 0xff 0xff 0xff]
      \f -17.25 [0x00 0x00 0x8a 0xc1]
      \f 0.0 [0x00 0x00 0x00 0x00]
      \f 5.75 [0x00 0x00 0xb8 0x40]
      \f Float/MAX_VALUE [0xff 0xff 0x7f 0x7f]
      \f Float/MIN_NORMAL [0x00 0x00 0x80 0x00]
      \f Float/MIN_VALUE [0x01 0x00 0x00 0x00]
      \f Float/NEGATIVE_INFINITY [0x00 0x00 0x80 0xff]
      \f Float/POSITIVE_INFINITY [0x00 0x00 0x80 0x7f]))

  (testing "NULL-terminated text"
    (are [?type ?values ?bytes]
         (= (map unchecked-byte ?bytes)
            (get-encoded-option-data ?type ?values))
      \Z "aaaBBB0011223344@@@+++"
      [0x61 0x61 0x61 0x42 0x42 0x42 0x30 0x30 0x31 0x31 0x32 0x32 0x33 0x33
       0x34 0x34 0x40 0x40 0x40 0x2b 0x2b 0x2b 0x00]
      \Z (str "!\"#$%&'()*+,-./0123456789:;<=>?@"
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")
      [0x21 0x22 0x23 0x24 0x25 0x26 0x27 0x28 0x29 0x2a 0x2b 0x2c 0x2d 0x2e
       0x2f 0x30 0x31 0x32 0x33 0x34 0x35 0x36 0x37 0x38 0x39 0x3a 0x3b 0x3c
       0x3d 0x3e 0x3f 0x40 0x41 0x42 0x43 0x44 0x45 0x46 0x47 0x48 0x49 0x4a
       0x4b 0x4c 0x4d 0x4e 0x4f 0x50 0x51 0x52 0x53 0x54 0x55 0x56 0x57 0x58
       0x59 0x5a 0x5b 0x5c 0x5d 0x5e 0x5f 0x60 0x61 0x62 0x63 0x64 0x65 0x66
       0x67 0x68 0x69 0x6a 0x6b 0x6c 0x6d 0x6e 0x6f 0x70 0x71 0x72 0x73 0x74
       0x75 0x76 0x77 0x78 0x79 0x7a 0x7b 0x7c 0x7d 0x7e 0x00]))

  (testing "array types"
    (are [?sub-type ?values ?bytes]
         (= (map unchecked-byte ?bytes)
            (get-encoded-option-data
             \B (cstr/join \, (cons ?sub-type ?values))))
      \c [0 127 -128 -1]
      [0x63                ;; c
       0x04 0x00 0x00 0x00 ;; 4
       0x00 0x7f 0x80 0xff]
      \C [0 127 128 255]
      [0x43                ;; C
       0x04 0x00 0x00 0x00 ;; 4
       0x00 0x7f 0x80 0xff]
      \s [0 32767 -32768 -1]
      [0x73                ;; s
       0x04 0x00 0x00 0x00 ;; 4
       0x00 0x00 0xff 0x7f
       0x00 0x80 0xff 0xff]
      \S [0 32767 32768 65535]
      [0x53                ;; S
       0x04 0x00 0x00 0x00 ;; 4
       0x00 0x00 0xff 0x7f
       0x00 0x80 0xff 0xff]
      \i [0 2147483647 -2147483648 -1]
      [0x69                ;; i
       0x04 0x00 0x00 0x00 ;; 4
       0x00 0x00 0x00 0x00
       0xff 0xff 0xff 0x7f
       0x00 0x00 0x00 0x80
       0xff 0xff 0xff 0xff]
      \I [0 2147483647 2147483648 4294967295]
      [0x49                ;; I
       0x04 0x00 0x00 0x00 ;; 4
       0x00 0x00 0x00 0x00
       0xff 0xff 0xff 0x7f
       0x00 0x00 0x00 0x80
       0xff 0xff 0xff 0xff]
      \f [-17.25 0.0 5.75 Float/MAX_VALUE]
      [0x66                ;; f
       0x04 0x00 0x00 0x00 ;; 4
       0x00 0x00 0x8a 0xc1
       0x00 0x00 0x00 0x00
       0x00 0x00 0xb8 0x40
       0xff 0xff 0x7f 0x7f])))

(deftest add-cigar-to-options-test
  (are [?cigar ?sample-option-list ?expected-option-list]
       (= ?expected-option-list (#'encoder/add-cigar-to-options ?sample-option-list ?cigar))
    "45S34N"
    '({:Xa {:type "A", :value \p}}
      {:XI {:type "B", :value "I,0,2147483647,4294967295"}})
    '({:CG {:type "B", :value "I,724,547"}}
      {:Xa {:type "A", :value \p}}
      {:XI {:type "B", :value "I,0,2147483647,4294967295"}})

    "1M2I4D8N16S32H64P128=65535X"
    '()
    '({:CG {:type "B", :value "I,16,33,66,131,260,517,1030,2055,1048568"}})))

(deftest encode-alignment-test
  (let [cigar-consisting-of-65535-operations (apply str "1S" (repeat 32767 "1I1M"))
        seq-for-65535-operations-cigar (apply str (repeat 65535 "A"))
        aln-65535-cigar-operations-byte
        (vec (concat
              [0 0 0 0 ;; refID
               0 0 0 0 ;; pos
               5 0 ;; l_read_name, mapq
               73 2 ;; bin
               -1 -1 ;; n_cigar_ops
               0 0 ;; flag
               -1 -1 0 0 ;;　l_seq
               -1 -1 -1 -1 ;; next_refID
               -1 -1 -1 -1 ;; next_pos
               0 0 0 0 ;; tlen
               82 48 48 49 0] ;; read_name
              ;; cigar
              [20 0 0 0] ;; 1S
              (take (* 8 32767) (cycle [17 0 0 0 16 0 0 0])) ;; repeat 1I1M
              (repeat 32767 17) '[16] ;; seq
              (repeat 65535 -1))) ;; qual
        cigar-consisting-of-65536-operations (apply str (repeat 32768 "1I1M"))
        seq-for-65536-operations-cigar (apply str (repeat 65536 "A"))
        aln-65536-cigar-operations-byte
        (vec (concat
              [0 0 0 0 ;; refID
               0 0 0 0 ;; pos
               5 0 ;; l_read_name, mapq
               73 2 ;; bin
               2 0 ;; n_cigar_op
               0 0 ;; flag
               0 0 1 0 ;; l_seq
               -1 -1 -1 -1 ;; next_refID
               -1 -1 -1 -1 ;; next_pos
               0 0 0 0 ;; tlen
               82 48 48 49 0 ;; read_name
               4 0 16 0 3 0 8 0] ;; cigar replaced by placeholder "65536S32768N"
              (repeat 32768 17) ;; seq
              (repeat 65536 -1) ;; qual
              ;; options
              [67 71 66 73] ;; tag: CG, val_type: B-I
              [0 0 1 0] ;; count
              (take (* 8 32768) (cycle [17 0 0 0 16 0 0 0])) ;; real cigar: repeat 1I1M
              ))]

    (are [?aln ?expected-byte]
         (= ?expected-byte
            (with-open [baos (ByteArrayOutputStream.)
                        dos (DataOutputStream. baos)]
              (encoder/encode-alignment dos ?aln '({:name "ref", :len 0}))
              (seq (.toByteArray baos))))

      (test-common/to-sam-alignment
       {:qname "r003", :flag 16, :rname "ref", :pos 29, :end 33, :mapq 30,
        :cigar "6H5M", :rnext "*", :pnext 0, :tlen 0, :seq "TAGGC", :qual "*",
        :options ()})
      [0 0 0 0 ;; refID
       28 0 0 0 ;; pos
       5 30 ;; l_read_name, mapq
       73 18 ;; bin
       2 0 ;; n_cigar_op
       16 0 ;; flag
       5 0 0 0 ;; l_seq
       -1 -1 -1 -1 ;; next_refID
       -1 -1 -1 -1 ;; next_pos
       0 0 0 0 ;; tlen
       114 48 48 51 0 ;; read_name
       101 0 0 0 80 0 0 0 ;; cigar
       -127 68 32 ;; seq
       -1 -1 -1 -1 -1] ;; qual

      (test-common/to-sam-alignment
       {:qname "R001" :flag (int 0) :rname "ref" :pos (int 1) :end (int 32767)
        :seq seq-for-65535-operations-cigar :qual "*"
        :cigar cigar-consisting-of-65535-operations
        :rnext "*" :pnext (int 0) :tlen (int 0) :mapq (int 0)
        :options ()})
      aln-65535-cigar-operations-byte

      (test-common/to-sam-alignment
       {:qname "R001" :flag (int 0) :rname "ref" :pos (int 1) :end (int 32768)
        :seq seq-for-65536-operations-cigar :qual "*"
        :cigar cigar-consisting-of-65536-operations
        :rnext "*" :pnext (int 0) :tlen (int 0) :mapq (int 0)
        :options ()})
      aln-65536-cigar-operations-byte)))

(ns cljam.io.bam.encoder-test
  "Tests for cljam.io.bam.encoder."
  (:require [clojure.test :refer [deftest are]]
            [cljam.io.bam.encoder :as bam-encoder]
            [cljam.test-common :as test-common])
  (:import [java.io ByteArrayOutputStream DataOutputStream]
           [java.nio ByteBuffer ByteOrder]))

(deftest long-cigar->placeholder
  (are [cigar placeholder]
       (= placeholder (#'bam-encoder/long-cigar->placeholder cigar))
    "1M2I4D8N16S32H64P128=256X" "403S397N"
    "10M20I100M200I1000M2000I" "3330S1110N"
    "12H30S140M5I32M" "207S172N"))

(deftest add-cigar-to-options
  (are [cigar expected-option-list sample-option-list]
       (= expected-option-list (#'bam-encoder/add-cigar-to-options sample-option-list cigar))

    "45S34N"

    '({:CG {:type "Z", :value "45S34N"}}
      {:Xa {:type "A", :value \p}}
      {:Xi {:type "i", :value -100}}
      {:Xf {:type "f", :value -1400.0}}
      {:Xz {:type "Z", :value "AATTGGCC"}}
      {:Xh {:type "H", :value (map unchecked-byte [0x1A 0xE3 0x01])}}
      {:Xc {:type "B", :value "c,-128,0,127"}}
      {:XC {:type "B", :value "C,0,127,255"}}
      {:Xs {:type "B", :value "s,-32768,0,32767"}}
      {:XS {:type "B", :value "S,0,32767,65535"}}
      {:Xi {:type "B", :value "i,-2147483648,0,2147483647"}}
      {:XI {:type "B", :value "I,0,2147483647,4294967295"}}
      {:Xf {:type "B", :value "f,-0.3,0.0,0.3"}})

    '({:Xa {:type "A", :value \p}}
      {:Xi {:type "i", :value -100}}
      {:Xf {:type "f", :value -1400.0}}
      {:Xz {:type "Z", :value "AATTGGCC"}}
      {:Xh {:type "H", :value (map unchecked-byte [0x1A 0xE3 0x01])}}
      {:Xc {:type "B", :value "c,-128,0,127"}}
      {:XC {:type "B", :value "C,0,127,255"}}
      {:Xs {:type "B", :value "s,-32768,0,32767"}}
      {:XS {:type "B", :value "S,0,32767,65535"}}
      {:Xi {:type "B", :value "i,-2147483648,0,2147483647"}}
      {:XI {:type "B", :value "I,0,2147483647,4294967295"}}
      {:Xf {:type "B", :value "f,-0.3,0.0,0.3"}})

    "1M2I4D8N16S32H64P128=256X"

    '({:CG {:type "Z", :value "1M2I4D8N16S32H64P128=256X"}})

    '()))

(def cigar-consisting-of-65535-operations (apply str "1S" (take 32767 (repeat "1I1M"))))
(def seq-for-65535-operations-cigar (apply str (take 65535 (repeat "A"))))
(def aln-65535-cigar-operations-byte
  (vec (concat
        '(-1 -1 -1 -1 -1 -1 -1 -1 5 0 73 2 -1 -1 0 0 -1 -1 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 82 48 48 49 0 20 0 0 0)
        (take (* 8 32767) (cycle [17 0 0 0 16 0 0 0]))
        (take 32767 (repeat 17))
        '(16)
        (take 65535 (repeat -1)))))

(def cigar-consisting-of-65536-operations (apply str (take 32768 (repeat "1I1M"))))
(def seq-for-65536-operations-cigar (apply str (take 65536 (repeat "A"))))
(def aln-65536-cigar-operations-byte
  (vec (concat
        '(-1 -1 -1 -1 -1 -1 -1 -1 5 0 73 2 2 0 0 0 0 0 1 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 82 48 48 49 0 4 0 16 0 3 0 8 0)
        (take 32768 (repeat 17))
        (take 65536 (repeat -1))
        '(67 71 90 49 73 49)
        (take (* 4 32767) (cycle [77 49 73 49]))
        '(77 0))))

(deftest encode-alignment-test
  (are [aln expected-byte]
       (= (doto (ByteBuffer/wrap (byte-array (map unchecked-byte expected-byte)))
            (.order ByteOrder/LITTLE_ENDIAN))
          (with-open [baos (ByteArrayOutputStream. 4096)
                      dos (DataOutputStream. baos)]
            (cljam.io.bam.encoder/encode-alignment dos aln '({:name "ref", :len 0}))
            (ByteBuffer/wrap (byte-array (.toByteArray baos)))))

    (test-common/to-sam-alignment {:qname "r003", :flag 16, :rname "ref", :pos 29, :end 33, :mapq 30, :cigar "6H5M", :rnext "*", :pnext 0, :tlen 0, :seq "TAGGC", :qual "*", :options ()})
    [0 0 0 0 28 0 0 0 5 30 73 18 2 0 16 0 5 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 51 0 101 0 0 0 80 0 0 0 -127 68 32 -1 -1 -1 -1 -1]

    (test-common/to-sam-alignment {:qname "R001" :flag (int 0) :rname "*" :pos (int 0) :end (int 10) :seq seq-for-65535-operations-cigar :qual "*" :cigar cigar-consisting-of-65535-operations :rnext "*" :pnext (int 0) :tlen (int 0) :mapq (int 0) :options '()})
    aln-65535-cigar-operations-byte

    (test-common/to-sam-alignment {:qname "R001" :flag (int 0) :rname "*" :pos (int 0) :end (int 10) :seq seq-for-65536-operations-cigar :qual "*" :cigar cigar-consisting-of-65536-operations :rnext "*" :pnext (int 0) :tlen (int 0) :mapq (int 0) :options '()})
    aln-65536-cigar-operations-byte))
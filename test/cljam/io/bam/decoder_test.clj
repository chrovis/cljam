(ns cljam.io.bam.decoder-test
  "Tests for cljam.io.bam.decoder."
  (:require [clojure.test :refer [deftest are]]
            [cljam.io.protocols]
            [cljam.io.bam.decoder :as bam-decoder]
            [cljam.test-common :as test-common]))

(deftest remove-cigar-from-options-test
  (are [expected-option sample-option]
       (= expected-option (#'bam-decoder/remove-cigar-from-options sample-option))
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

    '({:CG {:type "Z", :value "12S34N"}}
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

    '({:MD {:type "Z" :value "2G0C0"}}
      {:XT {:type "A", :value \R}}
      {:NM {:type "i", :value 44}}
      {:cg {:type "Z", :value "foo"}})

    '({:MD {:type "Z" :value "2G0C0"}}
      {:XT {:type "A", :value \R}}
      {:NM {:type "i", :value 44}}
      {:cg {:type "Z", :value "foo"}}
      {:CG {:type "Z", :value "34M"}})

    '()

    '({:CG {:type "Z", :value "34="}})

    '({:MD {:type "Z" :value "2G0C0"}}
      {:XT {:type "A", :value \R}}
      {:NM {:type "i", :value 44}}
      {:cg {:type "Z", :value "34M"}})

    '({:MD {:type "Z" :value "2G0C0"}}
      {:XT {:type "A", :value \R}}
      {:NM {:type "i", :value 44}}
      {:cg {:type "Z", :value "34M"}})))

((deftest first-cigar-operation-clips-entire-read?-test
   (are [expected cigar l-seq] (= expected (#'bam-decoder/first-cigar-operation-clips-entire-read? cigar l-seq))
     true "34S" 34
     true "1000000000S20N" 1000000000
     false "10S20N" 11
     false "10S" 11
     false "10H20N" 10)))

(deftest get-cigar-from-options-test
  (are [expected-cigar sample-option]
       (= expected-cigar (#'bam-decoder/get-cigar-from-options sample-option))
    "12S34N"
    '({:CG {:type "Z", :value "12S34N"}}
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

    "34M"
    '({:MD {:type "Z" :value "2G0C0"}}
      {:XT {:type "A", :value \R}}
      {:NM {:type "i", :value 44}}
      {:cg {:type "Z", :value "foo"}}
      {:CG {:type "Z", :value "34M"}})

    nil
    '({:MD {:type "Z" :value "2G0C0"}}
      {:XT {:type "A", :value \R}}
      {:NM {:type "i", :value 44}})))

(deftest two-arguments-decode-alignment-test
  (are [expected-aln byte]
       (= expected-aln (bam-decoder/decode-alignment [{:name "ref", :len 0}] {:data (byte-array (map unchecked-byte byte))}))
    (test-common/to-sam-alignment {:qname "r003", :flag 0, :rname "ref", :pos 9, :end 14, :mapq 30, :cigar "5H6M", :rnext "*", :pnext 0, :tlen 0, :seq "AGCTAA", :qual "*", :options ()})
    [0 0 0 0 8 0 0 0 5 30 73 18 2 0 0 0 6 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 51 0 85 0 0 0 96 0 0 0 20 40 17 -1 -1 -1 -1 -1 -1]

    (test-common/to-sam-alignment {:qname "r001", :flag 163, :rname "ref", :pos 7, :end 22, :mapq 30, :cigar "8M4I4M1D3M", :rnext "=", :pnext 37, :tlen 39, :seq "TTAGATAAAGAGGATACTG", :qual "*", :options ()})
    [0 0 0 0 6 0 0 0 5 30 73 18 2 0 -93 0 19 0 0 0 0 0 0 0 36 0 0 0 39 0 0 0 114 48 48 49 0 52 1 0 0 3 1 0 0 -120 20 24 17 20 20 65 -127 40 64 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 67 71 90 56 77 52 73 52 77 49 68 51 77 0]))

(deftest four-arguments-decode-alignment-test
  (are [expected-aln byte start end]
       (=  expected-aln (bam-decoder/decode-alignment [{:name "ref", :len 0}] {:data (byte-array (map unchecked-byte byte))} start end))
    (test-common/to-sam-alignment {:qname "r003", :flag 0, :rname "ref", :pos 9, :end 14, :mapq 30, :cigar "5H6M", :rnext "*", :pnext 0, :tlen 0, :seq "AGCTAA", :qual "*", :options ()})
    [0 0 0 0 8 0 0 0 5 30 73 18 2 0 0 0 6 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 51 0 85 0 0 0 96 0 0 0 20 40 17 -1 -1 -1 -1 -1 -1]
    14 9

    nil
    [0 0 0 0 8 0 0 0 5 30 73 18 2 0 0 0 6 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 51 0 85 0 0 0 96 0 0 0 20 40 17 -1 -1 -1 -1 -1 -1]
    14 8

    nil
    [0 0 0 0 8 0 0 0 5 30 73 18 2 0 0 0 6 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 114 48 48 51 0 85 0 0 0 96 0 0 0 20 40 17 -1 -1 -1 -1 -1 -1]
    15 9

    (test-common/to-sam-alignment {:qname "r001", :flag 163, :rname "ref", :pos 7, :end 22, :mapq 30, :cigar "8M4I4M1D3M", :rnext "=", :pnext 37, :tlen 39, :seq "TTAGATAAAGAGGATACTG", :qual "*", :options ()})
    [0 0 0 0 6 0 0 0 5 30 73 18 2 0 -93 0 19 0 0 0 0 0 0 0 36 0 0 0 39 0 0 0 114 48 48 49 0 52 1 0 0 3 1 0 0 -120 20 24 17 20 20 65 -127 40 64 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 67 71 90 56 77 52 73 52 77 49 68 51 77 0]
    22 7

    nil
    [0 0 0 0 6 0 0 0 5 30 73 18 2 0 -93 0 19 0 0 0 0 0 0 0 36 0 0 0 39 0 0 0 114 48 48 49 0 52 1 0 0 3 1 0 0 -120 20 24 17 20 20 65 -127 40 64 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 67 71 90 56 77 52 73 52 77 49 68 51 77 0]
    22 6

    nil
    [0 0 0 0 6 0 0 0 5 30 73 18 2 0 -93 0 19 0 0 0 0 0 0 0 36 0 0 0 39 0 0 0 114 48 48 49 0 52 1 0 0 3 1 0 0 -120 20 24 17 20 20 65 -127 40 64 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 67 71 90 56 77 52 73 52 77 49 68 51 77 0]
    23 7))
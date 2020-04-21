(ns cljam.io.sam.util.option-test
  (:require [clojure.test :refer [deftest is are testing]]
            [cljam.io.sam.util.option :as opt]))

(deftest parse-optional-field
  (testing "regression"
    (are [?str ?expect]
         (and (= (opt/parse-optional-field ?str) ?expect)
              (= (opt/stringify-optional-fields [?expect]) ?str))
      "AS:i:0" {:AS {:type "i" :value 0}}
      "BC:Z:CGTAC" {:BC {:type "Z" :value "CGTAC"}}
      "pa:f:0.987" {:pa {:type "f" :value 0.987}}
      "XX:A:A" {:XX {:type "A" :value \A}}
      "ZZ:B:i,1,2,3" {:ZZ {:type "B" :value "i,1,2,3"}}))
  (testing "hex"
    (is (= (update-in (opt/parse-optional-field "YY:H:1AE301") [:YY :value] seq)
           {:YY {:type "H" :value (map unchecked-byte [0x1A 0xE3 0x1])}}))))

(deftest stringify-optional-fields
  (is (= (opt/stringify-optional-fields [])
         ""))
  (is (= (opt/stringify-optional-fields [{:AS {:type "i" :value 0}} {:BC {:type "Z" :value "AAT"}}])
         "AS:i:0\tBC:Z:AAT")))

(deftest optional-fields
  (let [s ["NM:i:4"
           "MD:Z:103"
           "MC:Z:2S13M"
           "AS:i:103"
           "XS:i:103"
           "RG:Z:ReadGroup01"
           "BC:Z:CGTAC"
           "SA:Z:chr1,550,+,70S50M,0,4;chr1,550,-,40S30M50S,0,0;"
           "pa:f:0.987"
           "XA:Z:chr18,-80000000,10S90M10D20M10D10M,6;chr1,-240000000,10M10D70M10I10M10I10M10I10M,7;"
           "XR:Z:Reference Comment"]
        aln {:options (map opt/parse-optional-field s)}]
    (is (= (opt/score aln) 103))
    (is (= (opt/edit-distance aln) 4))
    (is (= (opt/mismatching-positions aln) [[:match 103]]))
    (is (= (opt/mate-cigar aln) "2S13M"))
    (is (= (opt/suboptimal-score aln) 103))
    (is (= (opt/read-group aln) "ReadGroup01"))
    (is (= (opt/barcode aln) "CGTAC"))
    (is (= (opt/supplementary-alignments aln)
           [{:rname "chr1", :pos 550, :strand :forward,
             :cigar "70S50M", :mapq 0, :edit-distance 4}
            {:rname "chr1", :pos 550, :strand :reverse,
             :cigar "40S30M50S", :mapq 0, :edit-distance 0}]))
    (is (= (opt/primary-to-alt-score aln) 0.987))
    (is (= (opt/alternative-hits aln)
           [{:rname "chr18", :pos -80000000, :cigar "10S90M10D20M10D10M", :edit-distance 6}
            {:rname "chr1", :pos -240000000, :cigar "10M10D70M10I10M10I10M10I10M", :edit-distance 7}]))
    (is (= (opt/ref-comment aln) "Reference Comment"))))

(deftest md-string
  (are [?str ?expect]
       (= (opt/parse-mismatching-positions-str ?str) ?expect)
    "100"
    [[:match 100]]
    "132T0"
    [[:match 132] [:mismatch \T] [:match 0]]
    "10A5^AC6"
    [[:match 10] [:mismatch \A] [:match 5] [:deletion "AC"] [:match 6]]
    "30C5C35C20"
    [[:match 30] [:mismatch \C] [:match 5] [:mismatch \C] [:match 35] [:mismatch \C] [:match 20]]
    "55^CTAACCCTAA63T8A2"
    [[:match 55] [:deletion "CTAACCCTAA"] [:match 63] [:mismatch \T] [:match 8] [:mismatch \A] [:match 2]]))

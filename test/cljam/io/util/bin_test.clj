(ns cljam.io.util.bin-test
  "Tests for cljam.io.util.bin."
  (:require [clojure.test :refer [deftest is are testing]]
            [cljam.test-common :refer
             [test-tabix-file
              test-csi-file]]
            [cljam.io.tabix :as tabix]
            [cljam.io.csi :as csi]
            [cljam.io.util.bin :as util-bin])
  (:import [clojure.lang ExceptionInfo]))

(deftest max-pos-test
  (are [?min-shift ?depth ?max-pos]
       (= ?max-pos (util-bin/max-pos ?min-shift ?depth))
    14 5 536870912
    15 5 1073741824
    0 0 1
    0 1 8
    0 2 64
    1 1 16
    1 2 128))

(deftest first-bin-of-level-test
  (are [?level ?first-bin]
       (= ?first-bin (util-bin/first-bin-of-level ?level))
    0 0
    1 1
    2 (+ 1 8)
    3 (+ 1 8 64)
    4 (+ 1 8 64 512)
    5 (+ 1 8 64 512 4096)
    6 (+ 1 8 64 512 4096 32768)))

(deftest bin-width-of-level-test
  (are [?level ?min-shift ?depth ?bin-width]
       (= ?bin-width (util-bin/bin-width-of-level ?level ?min-shift ?depth))
    0 14 5 536870912
    1 14 5 67108864
    2 14 5 8388608
    3 14 5 1048576
    4 14 5 131072
    5 14 5 16384
    0 15 5 1073741824
    0 0 0 1
    0 0 1 8
    1 0 1 1
    1 0 2 8))

(deftest bin-level-test
  (are [?bin ?level]
       (= ?level (util-bin/bin-level ?bin))
    0 0
    1 1
    2 1
    8 1
    9 2
    72 2
    73 3
    584 3
    585 4
    4680 4
    4681 5
    37448 5
    37449 6))

(deftest parent-bin-test
  (is (thrown? ExceptionInfo (util-bin/parent-bin 0)))
  (are [?bin ?parent-bin]
       (= ?parent-bin (util-bin/parent-bin ?bin))
    1 0
    8 0
    9 1
    16 1
    17 2
    72 8
    73 9
    584 72
    585 73
    4680 584
    4681 585
    4688 585
    4689 586))

(deftest bin-beg-test
  (are [?bin ?min-shift ?depth ?bin-beg]
       (= ?bin-beg (util-bin/bin-beg ?bin ?min-shift ?depth))
    4680 14 5 536739841
    4681 14 5 1
    4682 14 5 16385
    4683 14 5 32769)
  (doseq [index-shift [12 14 16]]
    (let [min-size (bit-shift-left 1 index-shift)]
      (is (= 1 (util-bin/bin-beg 4681 index-shift 5)))
      (is (= (+ 1 min-size) (util-bin/bin-beg 4682 index-shift 5)))
      (is (= (+ 1 (* min-size 2)) (util-bin/bin-beg 4683 index-shift 5)))
      (is (= 1 (util-bin/bin-beg 585 14 5)))
      (is (= (+ 1 (* min-size 8)) (util-bin/bin-beg 586 index-shift 5)))
      (is (= (+ 1 (* min-size 16)) (util-bin/bin-beg 587 index-shift 5)))
      (is (= (+ 1 (* min-size 24)) (util-bin/bin-beg 588 index-shift 5))))))

(deftest max-bin-test
  (are [?depth ?ans] (= (util-bin/max-bin ?depth) ?ans)
    1 8
    2 72
    3 584
    5 37448))

(deftest leading-bins-at-level-test
  (are [?pos ?level ?min-shift ?depth ?bins]
       (= ?bins (util-bin/leading-bins-at-level ?pos ?level ?min-shift ?depth))
    1 0 14 5 0
    1 5 14 5 0
    16385 5 14 5 1
    536870912 5 14 5 32768))

(deftest pos->lidx-offset-test
  (are [?pos ?linear-index-shift ?offset]
       (= ?offset (util-bin/pos->lidx-offset ?pos ?linear-index-shift))
    0 14 0
    1 14 0
    16384 14 0
    16385 14 1
    32768 14 1
    32769 14 2))

(deftest reg->bins-test
  (are [x] (= [0 1 9 73 585 4681]
              (util-bin/reg->bins 1 (bit-shift-left 1 x) x 5))
    13 14 15 16)
  (are [x] (= [0 1 9 73 585 4681 4682]
              (util-bin/reg->bins 1 (inc (bit-shift-left 1 x)) x 5))
    13 14 15 16)
  (is (= [0 2 17]
         (util-bin/reg->bins 9 9 0 2)))
  (is (= [0 1 9 73 585]
         (util-bin/reg->bins 1 1 14 4)))
  (is (= [0 1 9 73 585 4681]
         (util-bin/reg->bins 1 1 14 5)))
  (is (= [0 1 9 73 585 4681 37449]
         (util-bin/reg->bins 1 1 14 6)))
  (is (= [0 1 9 73 585 4681 37449 299593]
         (util-bin/reg->bins 1 1 14 7)))
  (is (= [0 1 9 73 585 4681 37449 37450]
         (util-bin/reg->bins 1 32769 15 6))))

(deftest reg->bin-test
  (testing "BAI-compatible"
    (are [?start ?end ?bin]
         (= ?bin
            (util-bin/reg->bin ?start ?end 14 5)
            ((set (util-bin/reg->bins ?start ?end 14 5)) ?bin))
      0 0 4681
      0 1 4681
      1 1 4681
      1 16384 4681
      16384 16384 4681
      16384 16385 585
      16385 16385 4682
      131072 131072 4688
      131072 131073 73
      8388608 8388609 1
      67108864 67108865 0
      536870912 536870912 37448
      536870912 536870913 37448))
  (testing "1-by-1"
    (are [?start ?end ?bin]
         (= ?bin
            (util-bin/reg->bin ?start ?end 0 2)
            ((set (util-bin/reg->bins ?start ?end 0 2)) ?bin))
      0 1 9
      1 1 9
      2 2 10
      1 2 1
      1 8 1
      1 9 0
      8 9 0
      9 9 17
      63 64 8
      64 64 72
      64 64 72))
  (testing "various min-shfits and depths"
    (is (= (util-bin/reg->bin 1 1 14 6) 37449))
    (is (= (util-bin/reg->bin 1 1 14 7) 299593))
    (is (= (util-bin/reg->bin 1 32769 15 6) 4681))
    (is (= (util-bin/reg->bin 240877561 240877568 14 6) 52150))))

(deftest get-spans-returns-a-sequence-including-regions
  (let [tabix* (tabix/read-index test-tabix-file)]
    (is (= [[0 50872]] (util-bin/get-spans tabix* 0 1 100)))
    (is (every? #(and (= 2 (count %))
                      (number? (first %))
                      (number? (second %)))
                (util-bin/get-spans tabix* 0 1 100))))
  (let [csi* (csi/read-index test-csi-file)]
    (is (= [[3904 3973]] (util-bin/get-spans csi* 0 1 100000)))
    (is (every? #(and (= 2 (count %))
                      (number? (first %))
                      (number? (second %)))
                (util-bin/get-spans csi* 0 1 100000)))))

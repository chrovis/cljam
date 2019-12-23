(ns cljam.io.util.bin-test
  "Tests for cljam.io.util.bin."
  (:require [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.tabix :as tabix]
            [cljam.io.csi :as csi]
            [cljam.io.util.bin :as util-bin]))

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

(deftest reg->bins-test
  (are [x] (= [0 1 9 73 585 4681]
              (#'util-bin/reg->bins 1 (bit-shift-left 1 x) x 5))
    13 14 15 16)
  (are [x] (= [0 1 9 73 585 4681 4682]
              (#'util-bin/reg->bins 1 (inc (bit-shift-left 1 x)) x 5))
    13 14 15 16)
  (is (= [0 2 17]
         (#'util-bin/reg->bins 9 9 0 2)))
  (is (= [0 1 9 73 585]
         (#'util-bin/reg->bins 1 1 14 4)))
  (is (= [0 1 9 73 585 4681]
         (#'util-bin/reg->bins 1 1 14 5)))
  (is (= [0 1 9 73 585 4681 37449]
         (#'util-bin/reg->bins 1 1 14 6)))
  (is (= [0 1 9 73 585 4681 37449 299593]
         (#'util-bin/reg->bins 1 1 14 7)))
  (is (= [0 1 9 73 585 4681 37449 37450]
         (#'util-bin/reg->bins 1 32769 15 6))))

(deftest bin-beg-test
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
  (are [depth ans] (= (util-bin/max-bin depth) ans)
    1 8
    2 72
    3 584
    5 37448))

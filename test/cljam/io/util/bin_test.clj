(ns cljam.io.util.bin-test
  "Tests for cljam.io.util.bin."
  (:require [clojure.test :refer :all]
            [cljam.test-common :refer :all]
            [cljam.io.tabix :as tabix]
            [cljam.io.util.bin :as util-bin]))

(deftest get-spans-returns-a-sequence-including-regions
  (let [tabix* (tabix/read-index test-tabix-file)]
    (is (= [[0 50872]] (util-bin/get-spans tabix* 0 0 100)))
    (is (every? #(and (= 2 (count %))
                      (number? (first %))
                      (number? (second %)))
                (util-bin/get-spans tabix* 0 0 100)))))

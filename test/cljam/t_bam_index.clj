(ns cljam.t-bam-index
  "Tests for cljam.bam-index."
  (:require [midje.sweet :refer :all]
            [cljam.t-common :refer :all]
            [cljam.bam-index :as bai]))

(fact "read-index is done without errors"
  (bai/read-index (bai/bam-index test-bai-file)) => anything)

(let [index (bai/read-index (bai/bam-index test-bai-file))]
  (fact "read-index returns vector"
    index => vector?)
  (fact "check the returning vector's structure"
    index => (has every? (just {:bin-index (has every? (just {:bin number?
                                                              :chunks (has every? (just {:beg number?
                                                                                         :end number?}))}))
                                :linear-index (has every? number?)}))))

(let [bai* (bai/bam-index test-bai-file)]
 (fact "get-spans returns a sequence including regions."
   (bai/get-spans bai* 0 0 100) => seq?
   (bai/get-spans bai* 0 0 100) => (has every? (just number? number?)))
 (fact "get-spans returns correct regions."
   (bai/get-spans bai* 0 0 100) => '((97 555))
   (bai/get-spans bai* 1 0 100) => '((555 29163520))))

;;; Tests for a large-size file.

(with-state-changes [(before :facts (prepare-cavia!))]
  (fact "read-index is done without errors with a large file" :slow :heavy
    (bai/read-index (bai/bam-index test-large-bai-file)) => anything))

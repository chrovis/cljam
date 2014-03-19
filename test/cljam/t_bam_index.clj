(ns cljam.t-bam-index
  "Tests for cljam.bam-index."
  (:require [midje.sweet :refer :all]
            [cljam.t-common :refer :all]
            [cljam.bam-index :as bai]))

;;; bin-index

(fact "bin-index is done without errors"
      (bai/bin-index test-bai-file 0) => anything)
(fact "bin-index throws BoundsException for the invalid given index"
      (bai/bin-index test-bai-file 2) => (throws Exception))
(fact "bin-index returns vector"
      (bai/bin-index test-bai-file 0) => vector?)
(fact "check the returning vector's structure"
      (bai/bin-index test-bai-file 0) => (has every? (just {:bin number?
                                                   :chunks (has every? (just {:beg number?
                                                                              :end number?}))})))

;;; linear-index

(fact "linear-index is done without errors"
      (bai/linear-index test-bai-file 0) => anything)
(fact "linear-index throws Exception for the invalid given index"
      (bai/linear-index test-bai-file 2) => (throws Exception))
(fact "linear-index returns vector"
      (bai/linear-index test-bai-file 0) => vector?)
(fact "check the returning vector's structure"
      (bai/linear-index test-bai-file 0) => (has every? number?))

;;; get-spans

(let [bai* (bai/bam-index test-bai-file)]
 (fact "get-spans returns a sequence including regions."
   (bai/get-spans bai* 0 0 100) => seq?
   (bai/get-spans bai* 0 0 100) => (has every? (just number? number?)))
 (fact "get-spans returns correct regions."
   (bai/get-spans bai* 0 0 100) => '((97 555))
   (bai/get-spans bai* 1 0 100) => '((555 29163520))))

;;; Tests for a large-size file.

(with-state-changes [(before :facts (prepare-cavia!))]
  (fact "bin-index is done without errors with a large file" :slow :heavy
    (bai/bin-index (bai/bam-index test-large-bai-file) 0) => anything)
  (fact "linear-index is done without errors with a large file" :slow :heavy
    (bai/linear-index (bai/bam-index test-large-bai-file) 0) => anything))

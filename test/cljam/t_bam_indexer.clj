(ns cljam.t-bam-indexer
  (:require [midje.sweet :refer :all]
            [cljam.t-common :refer :all]
            [cljam.bam-indexer :as bai]))

(fact "read-index is done without errors"
  (bai/read-index test-bai-file) => anything)

(let [index (bai/read-index test-bai-file)]
  (fact "read-index returns vector"
    index => vector?)
  (fact "check the returning vector's structure"
    index => (has every? (just {:bin-index (has every? (just {:bin number?
                                                              :chunks (has every? (just {:beg number?
                                                                                         :end number?}))}))
                                :linear-index (has every? number?)}))))

(with-state-changes [(before :facts (prepare-cavia!))]
  (fact "read-index is done without errors with a large file" :slow :heavy
    (bai/read-index test-large-bai-file) => anything))

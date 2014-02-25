(ns cljam.t-fasta-indexer
  "Tests for cljam.fasta-indexer."
  (:require [clojure.java.io :as io]
            [midje.sweet :refer :all]
            [cljam.t-common :refer :all]
            [cljam.fasta-indexer :as fai]))

;;; create-index!

(with-state-changes [(before :facts (prepare-cache!))
                     (after :facts (clean-cache!))]
  (let [out-fai (str temp-dir "/test.fa.fai") ]
    (fact "about create-index!"
      "create-index! is done without errors"
      (fai/create-index! test-fa-file out-fai) => anything

      "the output FASTA index file exists"
      (.exists (io/file out-fai)) => truthy

      "the output is same as test.fa.fai"
      (slurp out-fai) => (slurp test-fai-file))))

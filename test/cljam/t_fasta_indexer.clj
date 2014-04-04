(ns cljam.t-fasta-indexer
  "Tests for cljam.fasta-indexer."
  (:require [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [midje.sweet :refer :all]
            [cljam.t-common :refer :all]
            [cljam.fasta-indexer :as fai]))

(def temp-fa-file (str temp-dir "/test.fai"))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (fs/copy test-fa-file temp-fa-file)))
                     (after :facts (clean-cache!))]
  (fact "about FASTA indexer"
    (fai/create-index! temp-fa-file (str temp-fa-file ".fai")) => anything
    (fs/exists? (str temp-fa-file ".fai")) => truthy
    (same-file? (str temp-fa-file ".fai") test-fai-file) => truthy))

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (fs/copy medium-fa-file temp-fa-file)))
                     (after :facts (clean-cache!))]
  (fact "about FASTA indexer (medium file)" :slow
    (fai/create-index! temp-fa-file (str temp-fa-file ".fai")) => anything
    (fs/exists? (str temp-fa-file ".fai")) => truthy
    (same-file? (str temp-fa-file ".fai") medium-fai-file) => truthy))

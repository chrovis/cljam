(ns cljam.t-fasta-indexer
  "Tests for cljam.fasta-indexer."
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [cljam.t-common :refer :all]
            [cljam.fasta-indexer :as fai]))

(def temp-fa-file (str temp-dir "/test.fa"))

(deftest about-fasta-indexer
  (with-before-after {:before (do (prepare-cache!)
                                  (fs/copy test-fa-file temp-fa-file))
                      :after (clean-cache!)}
    (is (not-throw? (fai/create-index temp-fa-file (str temp-fa-file ".fai"))))
    (is (fs/exists? (str temp-fa-file ".fai")))
    (is (same-file? (str temp-fa-file ".fai") test-fai-file))))

(deftest-slow about-fasta-indexer-medium-file
  (with-before-after {:before (do (prepare-cache!)
                                  (fs/copy medium-fa-file temp-fa-file))
                      :after (clean-cache!)}
    (is (not-throw? (fai/create-index temp-fa-file (str temp-fa-file ".fai"))))
    (is (fs/exists? (str temp-fa-file ".fai")))
    (is (same-file? (str temp-fa-file ".fai") medium-fai-file))))

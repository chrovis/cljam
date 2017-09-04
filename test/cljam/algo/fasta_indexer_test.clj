(ns cljam.algo.fasta-indexer-test
  "Tests for cljam.fasta-indexer."
  (:require [clojure.test :refer :all]
            [clojure.java.io :as cio]
            [cljam.test-common :refer :all]
            [cljam.algo.fasta-indexer :as fai]))

(def temp-fa-file (str temp-dir "/test.fa"))

(deftest about-fasta-indexer
  (with-before-after {:before (do (prepare-cache!)
                                  (cio/copy (cio/file test-fa-file)
                                            (cio/file temp-fa-file)))
                      :after (clean-cache!)}
    (is (not-throw? (fai/create-index temp-fa-file (str temp-fa-file ".fai"))))
    (is (.isFile (cio/file (str temp-fa-file ".fai"))))
    (is (same-file? (str temp-fa-file ".fai") test-fai-file))))

(deftest-slow about-fasta-indexer-medium-file
  (with-before-after {:before (do (prepare-cache!)
                                  (cio/copy (cio/file medium-fa-file)
                                            (cio/file temp-fa-file)))
                      :after (clean-cache!)}
    (is (not-throw? (fai/create-index temp-fa-file (str temp-fa-file ".fai"))))
    (is (.isFile (cio/file (str temp-fa-file ".fai"))))
    (is (same-file? (str temp-fa-file ".fai") medium-fai-file))))

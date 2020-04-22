(ns cljam.algo.fasta-indexer-test
  "Tests for cljam.fasta-indexer."
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as cio]
            [cljam.test-common :as common]
            [cljam.algo.fasta-indexer :as fai]))

(def temp-fa-file (str common/temp-dir "/test.fa"))

(deftest about-fasta-indexer
  (common/with-before-after
    {:before (do (common/prepare-cache!)
                 (cio/copy (cio/file common/test-fa-file)
                           (cio/file temp-fa-file)))
     :after (common/clean-cache!)}
    (is (common/not-throw?
         (fai/create-index temp-fa-file (str temp-fa-file ".fai"))))
    (is (.isFile (cio/file (str temp-fa-file ".fai"))))
    (is (common/same-file? (str temp-fa-file ".fai") common/test-fai-file))))

(common/deftest-slow about-fasta-indexer-medium-file
  (common/with-before-after
    {:before (do (common/prepare-cache!)
                 (cio/copy (cio/file common/medium-fa-file)
                           (cio/file temp-fa-file)))
     :after (common/clean-cache!)}
    (is (common/not-throw?
         (fai/create-index temp-fa-file (str temp-fa-file ".fai"))))
    (is (.isFile (cio/file (str temp-fa-file ".fai"))))
    (is (common/same-file? (str temp-fa-file ".fai") common/medium-fai-file))))

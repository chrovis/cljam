(ns cljam.t-dict
  "Tests for cljam.dict."
  (:require [midje.sweet :refer :all]
            [me.raynes.fs :as fs]
            [cljam.t-common :refer :all]
            [cljam.dict :as dict]))

;; Resources
;; ---------

(def temp-fa-file (str temp-dir "/test.fa"))
(def out-dict-file (str temp-fa-file ".dict"))

;; Small-size FASTA
;; ---------------------------------

(with-state-changes [(before :facts (do (prepare-cache!)
                                        (fs/copy test-fa-file temp-fa-file)))
                     (after :facts (clean-cache!))]
  (fact "about create-dict"
    ;; Create dictionary without errors
    (dict/create-dict temp-fa-file out-dict-file) => anything
    ;; Check the file existence
    (fs/exists? out-dict-file) => truthy))

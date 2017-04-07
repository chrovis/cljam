(ns cljam.t-dict
  "Tests for cljam.dict."
  (:require [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [cljam.t-common :refer :all]
            [cljam.dict :as dict]))

;; Resources
;; ---------

(def temp-fa-file (str temp-dir "/test.fa"))
(def out-dict-file (str temp-fa-file ".dict"))

;; Small-size FASTA
;; ---------------------------------

(deftest about-create-dict
  (with-before-after {:before (do (prepare-cache!)
                                  (fs/copy test-fa-file temp-fa-file))
                      :after (clean-cache!)}
    ;; Create dictionary without errors
    (is (not-throw? (dict/create-dict temp-fa-file out-dict-file)))
    ;; Check the file existence
    (is (fs/exists? out-dict-file))))

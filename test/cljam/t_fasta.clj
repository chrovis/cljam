(ns cljam.t-fasta
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.fasta :as fasta]))

;; (fact "about slurp-fasta"
;;   (fa/slurp test-fa-file) => test-fa)

(fact "about read FASTA file"
  (let [rdr (fasta/reader test-fa-file)]
    (fasta/read-headers rdr)) => test-fa-header
  (let [rdr (fasta/reader test-fa-file)]
    (fasta/read-sequences rdr)) => test-fa-sequences
  (let [rdr (fasta/reader test-fa-file)]
    (fasta/read-sequence rdr "ref" 4 10) => "TGTTAGA"
  (let [rdr (fasta/reader test-fa-file)]
    (fasta/read-sequence rdr "ref2" 0 16) => "AGGTTTTATAAAACAAT")))

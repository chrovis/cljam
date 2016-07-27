(ns cljam.t-fasta
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.fasta :as fasta]
            [clojure.string :as str]))

;; (fact "about slurp-fasta"
;;   (fa/slurp test-fa-file) => test-fa)

(fact "about read FASTA file"
  (let [rdr (fasta/reader test-fa-file)]
    (fasta/read-headers rdr)) => test-fa-header
  (let [rdr (fasta/reader test-fa-file)]
    (fasta/read-sequences rdr)) => test-fa-sequences
  (let [rdr (fasta/reader test-fa-file)]
    (fasta/read-sequence rdr "ref" 4 10) => "TGTTAG"
  (let [rdr (fasta/reader test-fa-file)]
    (fasta/read-sequence rdr "ref2" 0 16) => "AGGTTTTATAAAACAA")
  (let [rdr (fasta/reader test-fa-file)]
    (fasta/read-sequence rdr "ref") => "AGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT")
  (let [rdr (fasta/reader test-fa-file)]
    (fasta/read-sequence rdr "ref2") => "AGGTTTTATAAAACAATTAAGTCTACAGAGCAACTACGCG")))

(fact "about sequential reading of FASTA file."
  (fasta/sequential-read test-fa-file)     => (map #(update % :sequence str/upper-case) test-fa-sequences)
  (fasta/sequential-read test-fa-bz2-file) => (map #(update % :sequence str/upper-case) test-fa-sequences)
  (let [fa (fasta/sequential-read medium-fa-file)]
    (map :name fa) => '("chr1")
    (map (comp count :sequence) fa) => '(100000))
  (let [fa (fasta/sequential-read medium-fa-gz-file)]
    (map :name fa) => '("chr1")
    (map (comp count :sequence) fa) => '(100000)))

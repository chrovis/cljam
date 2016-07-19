(ns cljam.t-pileup
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.bam :as bam]
            [cljam.fasta :as fa]
            [cljam.pileup :as plp]))

(def test-bam-pileup-ref '(0 0 0 0 0 0 0 1 1 3 3 3 3 3 3 2 3 3 3 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 1 1 1 2 2 2 2 1 1 1 1 1))
(def test-bam-pileup-ref2 '(0 1 2 2 2 2 3 3 3 3 4 4 5 5 6 6 6 6 6 6 6 5 5 4 4 4 4 4 3 3 3 3 3 3 3 2 1 0 0 0 0))
(def test-bam-mpileup-seq-ref
  '(() () () () () () () ("T") ("T") ("A" "A" "A") ("G" "G" "G") ("A" "A" "C") ("T" "T" "T")
    ("A" "A" "A") ("+4AGAG" "+2GG" "A") ("G" "G") ("A" "A" "A") ("T" "T" "T") ("A" "+2AA" "A")
    ("*" "G") ("C" "C") ("T" "T") ("G" ">") (">") (">") (">") (">") (">") (">") (">" "T") (">" "A")
    (">" "G") (">" "G") (">" "C") (">") ("+1C") ("T") ("C" "C") ("A" "A") ("G" "G") ("C" "C")
    ("G") ("C") ("C") ("A") ("T")))
(def test-bam-mpileup-seq-ref2
  '(() () () () () () () (".") (".") ("." "." ".") ("." "." ".") ("." "." "C") ("." "." ".")
    ("." "." ".") ("+4AGAG" "+2GG" ".") ("." ".") ("." "." ".") ("." "." ".") ("." "+2AA" ".")
    ("*" ".") ("." ".") ("." ".") ("." ">") (">") (">") (">") (">") (">") (">") (">" ".") (">" ".")
    (">" ".") (">" ".") (">" ".") (">") ("+1C") (".") ("." ".") ("." ".") ("." ".") ("." ".")
    (".") (".") (".") (".") (".")))
(def test-bam-mpileup-qual-ref
  '([] [] [] [] [] [] [] [\~] [\~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~]
    [\~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~ \~] [\~ \~] [\~ \~] [\~ \~] [\~ \~] [\~] [\~] [\~] [\~]
    [\~] [\~] [\~ \~] [\~ \~] [\~ \~] [\~ \~] [\~ \~] [\~] [\~] [\~] [\~ \~] [\~ \~] [\~ \~] [\~ \~]
    [\~] [\~] [\~] [\~] [\~]))

(fact "pileup returns LazySeq"
  (type (plp/pileup (bam/reader test-sorted-bam-file) "ref" nil)) => clojure.lang.LazySeq)

(fact "pileup returns LazySeq"
  (type (plp/pileup (bam/reader test-sorted-bam-file) "ref2" nil)) => clojure.lang.LazySeq)

(fact "about pileup"
  (plp/pileup (bam/reader test-sorted-bam-file) "ref" nil) => test-bam-pileup-ref)

(fact "about pileup"
  (plp/pileup (bam/reader test-sorted-bam-file) "ref2" nil) => test-bam-pileup-ref2)

(fact "about first-pos"
  (plp/first-pos (bam/reader test-sorted-bam-file) "ref" 0 64) => 7)

(fact "about first-pos"
  (plp/first-pos (bam/reader test-sorted-bam-file) "ref2" 0 64) => 1)

(fact "about mpileup"
  (with-open [br (bam/reader test-sorted-bam-file)
              fr (fa/reader test-fa-file)]
    (let [mplp-ref (doall (plp/mpileup br "ref"))
          mplp-ref2 (doall (plp/mpileup br "ref2"))]
      (map :rname mplp-ref) => (repeat 46 "ref")
      (map :ref mplp-ref) => (repeat 46 \N)
      (map :count mplp-ref) => test-bam-pileup-ref
      (map :pos mplp-ref) => (range 46)
      (map :seq mplp-ref) => test-bam-mpileup-seq-ref
      (map :qual mplp-ref) => test-bam-mpileup-qual-ref
      (map :count mplp-ref2) => test-bam-pileup-ref2)

    (let [mplp-ref (doall (plp/mpileup fr br "ref"))]
      (map :rname mplp-ref) => (repeat 46 "ref")
      (apply str (map :ref mplp-ref)) => "NAGCATGTTAGATAAGATAGCTGTGCTAGTAGGCAGTCAGCGCCAT"
      (map :count mplp-ref) => test-bam-pileup-ref
      (map :pos mplp-ref) => (range 46)
      (map :seq mplp-ref) => test-bam-mpileup-seq-ref2
      (map :qual mplp-ref) => test-bam-mpileup-qual-ref)))


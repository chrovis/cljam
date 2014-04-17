(ns cljam.t-pileup
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.bam :as bam]
            [cljam.pileup :as plp]))

(def test-bam-pileup-ref '(0 0 0 0 0 0 0 1 1 3 3 3 3 3 3 2 3 3 3 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 1 1 1 2 2 2 2 1 1 1 1 1))
(def test-bam-pileup-ref2 '(0 1 2 2 2 2 3 3 3 3 4 4 5 5 6 6 6 6 6 6 6 5 5 4 4 4 4 4 3 3 3 3 3 3 3 2 1 0 0 0 0))

(fact "pileup returns LazySeq"
  (type (plp/pileup (bam/reader test-sorted-bam-file) "ref")) => clojure.lang.LazySeq)

(fact "pileup returns LazySeq"
  (type (plp/pileup (bam/reader test-sorted-bam-file) "ref2")) => clojure.lang.LazySeq)

(fact "about pileup"
  (plp/pileup (bam/reader test-sorted-bam-file) "ref") => test-bam-pileup-ref)

(fact "about pileup"
  (plp/pileup (bam/reader test-sorted-bam-file) "ref2") => test-bam-pileup-ref2)

(fact "about first-pos"
  (plp/first-pos (bam/reader test-sorted-bam-file) "ref" 0 64) => 7)

(fact "about first-pos"
  (plp/first-pos (bam/reader test-sorted-bam-file) "ref2" 0 64) => 1)

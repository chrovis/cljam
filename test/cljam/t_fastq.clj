(ns cljam.t-fastq
  (:use midje.sweet
        cljam.t-common)
  (:require [cljam.fastq :as fq]))

(fact
 "about FASTQ file"
 (with-open [r (fq/reader test-fq-file)]
   (doall (fq/read-sequence r))) => test-fq-sequences
 (with-open [r (fq/reader test-fq-file)]
   (doall (fq/read-sequence r :decode-quality false))) => test-fq-sequences-raw)

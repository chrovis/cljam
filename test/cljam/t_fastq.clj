(ns cljam.t-fastq
  (:use midje.sweet
        cljam.t-common)
  (:require [clojure.java.io :as io]
            [cljam.fastq :as fq]))

(fact
 "FASTQ file input"
 (with-open [r (fq/reader test-fq-file)]
   (doall (fq/read-sequence r))) => test-fq-sequences
 (with-open [r (fq/reader test-fq-file)]
   (doall (fq/read-sequence r :decode-quality false))) => test-fq-sequences-raw
 (with-open [r (fq/reader test-fq-gz-file)]
   (doall (fq/read-sequence r))) => test-fq-sequences
 (with-open [r (fq/reader test-fq-bz2-file)]
   (doall (fq/read-sequence r))) => test-fq-sequences)

(with-state-changes
  [(before
    :facts
    (do (clean-cache!)
        (prepare-cache!)))
   (after
    :facts
    (clean-cache!))]
  (fact
   "FASTQ file output"
   (let [path (.getAbsolutePath (io/file temp-dir "test.fq"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences))
     (with-open [r (fq/reader path)]
       (doall (fq/read-sequence r))))
   => test-fq-sequences

   (let [path (.getAbsolutePath (io/file temp-dir "test.fq.gz"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences))
     (with-open [r (fq/reader path)]
       (doall (fq/read-sequence r))))
   => test-fq-sequences

   (let [path (.getAbsolutePath (io/file temp-dir "test.fq.bz2"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences))
     (with-open [r (fq/reader path)]
       (doall (fq/read-sequence r))))
   => test-fq-sequences))

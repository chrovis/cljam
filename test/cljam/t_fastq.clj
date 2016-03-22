(ns cljam.t-fastq
  (:use midje.sweet
        cljam.t-common)
  (:require [clojure.java.io :as io]
            [cljam.fastq :as fq]))

(fact
 "FASTQ file input"
 (with-open [r (fq/reader test-fq-file)]
   (doall (map (partial into {}) (fq/read-sequence r)))) => test-fq-sequences
 (with-open [r (fq/reader test-fq-file)]
   (doall (map (partial into {}) (fq/read-sequence r :decode-quality :phred33)))) => test-fq-sequences
 (with-open [r (fq/reader test-fq-file)]
   (doall (map (partial into {}) (fq/read-sequence r :decode-quality nil)))) => test-fq-sequences-raw
 (with-open [r (fq/reader test-fq-file)]
   (doall (map (partial into {}) (fq/read-sequence r :decode-quality :phred64)))) => throws AssertionError
 (with-open [r (fq/reader test-fq-gz-file)]
   (doall (map (partial into {}) (fq/read-sequence r)))) => test-fq-sequences
 (with-open [r (fq/reader test-fq-bz2-file)]
   (doall (map (partial into {}) (fq/read-sequence r)))) => test-fq-sequences)

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
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => test-fq-sequences

   (let [path (.getAbsolutePath (io/file temp-dir "test-33.fq"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences :encode-quality :phred33))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => test-fq-sequences

   (let [path (.getAbsolutePath (io/file temp-dir "test-raw.fq"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences-raw :encode-quality nil))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => test-fq-sequences

   (let [path (.getAbsolutePath (io/file temp-dir "test-64.fq"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences :encode-quality :phred64))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => throws AssertionError

   (let [path (.getAbsolutePath (io/file temp-dir "test.fq.gz"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => test-fq-sequences

   (let [path (.getAbsolutePath (io/file temp-dir "test.fq.bz2"))]
     (with-open [w (fq/writer path)]
       (fq/write-sequence w test-fq-sequences))
     (with-open [r (fq/reader path)]
       (doall (map (partial into {}) (fq/read-sequence r)))))
   => test-fq-sequences))

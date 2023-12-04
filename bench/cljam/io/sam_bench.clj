(ns cljam.io.sam-bench
  (:require [libra.bench :refer [defbench are]]
            [libra.criterium :as c]
            [clojure.java.io :as cio]
            [cljam.test-common :as tcommon]
            [cljam.util :as util]
            [cljam.io.sam :as sam]))

(defbench encode-alignment-short-bench
  (are [f]
       (util/with-temp-dir [d "encode-alignment-short-bench"]
         (with-open [r (sam/reader f)
                     w (sam/writer (cio/file d "out.bam"))]
           (let [header (sam/read-header r)
                 xs (vec (sam/read-alignments r))]
             (sam/write-header w header)
             (sam/write-refs w header)
             (doseq [x xs o (:options x)] o)
             (c/quick-bench
              (sam/write-alignments w xs header)))))
    tcommon/test-sam-file
    tcommon/medium-sam-file))

(defbench encode-alignment-long-bench
  (tcommon/prepare-cavia!)
  (are [f]
       (util/with-temp-dir [d "encode-alignment-long-bench"]
         (with-open [r (sam/reader f)
                     w (sam/writer (cio/file d "out.bam"))]
           (let [header (sam/read-header r)
                 xs (vec (take 2000000 (sam/read-alignments r)))]
             (sam/write-header w header)
             (sam/write-refs w header)
             (doseq [x xs o (:options x)] o)
             (c/quick-bench
              (sam/write-alignments w xs header)))))
    tcommon/large-bam-file))

(defbench decode-bam-alignment-short-bench
  (are [f decode-opts?]
       (c/quick-bench
        (with-open [r (sam/reader f)]
          (run! (fn [aln]
                  (when decode-opts?
                    (dorun (:options aln))))
                (sam/read-alignments r))))
    tcommon/test-bam-file false
    tcommon/test-bam-file true
    tcommon/medium-bam-file false
    tcommon/medium-bam-file true))

(defbench decode-bam-alignment-long-bench
  (tcommon/prepare-cavia!)
  (are [f]
       (c/quick-bench
        (with-open [r (sam/reader f)]
          (transduce (take 10000000)
                     (completing #(dorun (:options %2)))
                     nil
                     (sam/read-alignments r))))
    tcommon/large-bam-file))

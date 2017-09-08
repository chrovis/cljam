(ns cljam.algo.depth-bench
  (:require [libra.bench :refer :all]
            [libra.criterium :as c]
            [cljam.test-common :as tcommon]
            [cljam.algo.depth :as depth]
            [cljam.io.sam :as sam]))

(defbench depth-medium-bench
  (are [n] (c/quick-bench (with-open [rdr (sam/reader tcommon/medium-bam-file)]
                            (depth/depth rdr {:chr "chr1" :start 1000000 :end 9999999} {:n-threads n})))
    1
    2
    4))

;; Simple pileup benchmark equivalent to being used in the SCfBM paper,
;; https://doi.org/10.1186/s13029-016-0058-6.
;; Note that it requires stupendous time and CPU/RAM resources.
(when-let [bam (System/getenv "SCFBM_BENCH_BAM")]
  (defbench lazy-depth-scfbm-bench
    (are [n] (dur 10 (with-open [rdr (sam/reader bam)]
                       (dorun (depth/lazy-depth rdr {:chr "1"} {:n-threads n}))))
      1
      2
      3
      4
      6
      8
      12)))

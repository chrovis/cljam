(ns cljam.algo.bam-indexer-bench
  (:require [libra.bench :refer :all]
            [libra.criterium :as c]
            [cljam.test-common :as tcommon]
            [cljam.algo.bam-indexer :as bai]))

(defbench create-index-medium-bench
  (tcommon/with-before-after {:before (tcommon/prepare-cache!)
                              :after (tcommon/clean-cache!)}
    (are [n] (c/quick-bench (bai/create-index tcommon/medium-bam-file
                                              (str tcommon/temp-dir "/out-" n ".bai")
                                              :n-threads n))
      1
      2
      4)))

;; BAM-indexing benchmark equivalent to being used in the SCfBM paper,
;; https://doi.org/10.1186/s13029-016-0058-6.
;; Note that it requires stupendous time and CPU resources.
(when-let [bam (System/getenv "SCFBM_BENCH_BAM")]
  (tcommon/with-before-after {:before (tcommon/prepare-cache!)
                              :after (tcommon/clean-cache!)}
    (defbench create-index-scfbm-bench
      (are [n] (dur 10 (bai/create-index bam
                                         (str tcommon/temp-dir "/out-" n ".bai")
                                         :n-threads n))
        1
        2
        3
        4
        6
        8
        12))))

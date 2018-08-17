(ns cljam.algo.pileup-bench
  (:require [libra.bench :refer :all]
            [libra.criterium :as c]
            [criterium.core :as criterium]
            [cljam.test-common :as tcommon]
            [cljam.algo.pileup :as pileup]
            [cljam.io.sam :as sam]))

(defbench mpileup-large-bench
  (tcommon/with-before-after {:before (tcommon/prepare-cavia!)}
    (let [region {:chr "chr1"}]
      (with-open [r (sam/reader tcommon/large-bam-file)]
        (dorun (sam/read-alignments r region)) ;; warm up
        (criterium/with-progress-reporting
          (is
           (c/quick-bench
            (dorun (pileup/pileup r region)))))))))


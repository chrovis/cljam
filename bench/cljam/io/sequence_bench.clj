(ns cljam.io.sequence-bench
  (:require [libra.bench :refer :all]
            [libra.criterium :as c]
            [cljam.test-common :as tcommon]
            [cljam.io.sequence :as cseq]))

(defbench read-all-sequences-test
  (are [f opts] (c/quick-bench (with-open [rdr (cseq/reader f)]
                                 (dorun (cseq/read-all-sequences rdr opts))))
    tcommon/medium-fa-file {}
    tcommon/medium-fa-file {:mask? true}
    tcommon/medium-twobit-file {}
    tcommon/medium-twobit-file {:mask? true}))

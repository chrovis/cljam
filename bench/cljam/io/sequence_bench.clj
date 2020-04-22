(ns cljam.io.sequence-bench
  (:require [libra.bench :refer [defbench are]]
            [libra.criterium :as c]
            [cljam.test-common :as tcommon]
            [cljam.io.sequence :as cseq]))

(defbench read-all-sequences-test
  (are [f opts] (c/quick-bench (with-open [rdr (cseq/reader f)]
                                 (dorun (cseq/read-all-sequences rdr opts))))
    tcommon/medium-fa-file {}
    tcommon/medium-fa-file {:mask? true}
    tcommon/medium-fa-bgz-file {}
    tcommon/medium-fa-bgz-file {:mask? true}
    tcommon/medium-twobit-file {}
    tcommon/medium-twobit-file {:mask? true}))

(defbench read-sequence-once-test
  (are [f opts]
      (let [region {:chr "chr7",
                    :start 10000,
                    :end 45000}]
        (c/quick-bench
         (with-open [rdr (cseq/reader f)]
           (cseq/read-sequence rdr region opts))))
    tcommon/medium-fa-file {}
    tcommon/medium-fa-file {:mask? true}
    tcommon/medium-fa-bgz-file {}
    tcommon/medium-fa-bgz-file {:mask? true}
    tcommon/medium-twobit-file {}
    tcommon/medium-twobit-file {:mask? true}))

(defbench read-sequence-test
  (are [f opts]
      (let [region {:chr "chr7",
                    :start 10000,
                    :end 45000}]
        (with-open [rdr (cseq/reader f)]
          ;; warm up
          (cseq/read-sequence rdr region opts)
          (c/quick-bench
           (cseq/read-sequence rdr region opts))))
    tcommon/medium-fa-file {}
    tcommon/medium-fa-file {:mask? true}
    tcommon/medium-fa-bgz-file {}
    tcommon/medium-fa-bgz-file {:mask? true}
    tcommon/medium-twobit-file {}
    tcommon/medium-twobit-file {:mask? true}))

(ns cljam.algo.convert-bench
  (:require [libra.bench :refer :all]
            [libra.criterium :as c]
            [cljam.test-common :as tcommon]
            [cljam.algo.convert :as convert]))

(defbench convert-medium-bench
  (tcommon/with-before-after {:before (tcommon/prepare-cache!)
                              :after (tcommon/clean-cache!)}
    (are [n] (c/quick-bench (convert/convert tcommon/medium-sam-file
                                             (str tcommon/temp-dir "/out-" n ".bam")
                                             :n-threads n
                                             :num-block 1000))
      1
      2
      4)))

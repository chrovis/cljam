(ns cljs-tdd.test.core-runner
  (:require ;; [cljs.test :as test]
            [doo.runner :refer-macros [doo-all-tests doo-tests]]
            [cljam.t-cigar]))

(doo-tests 'cljam.t-cigar)

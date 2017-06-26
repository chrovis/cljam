(ns cljam.t-util
  "Tests for cljam.util."
  (:require [clojure.test :refer :all]
            [cljam.util :as util]))

(deftest ubyte
  (are [?n ?expected] (= (util/ubyte ?n) ?expected)
    0    (byte 0)
    127  (byte 127)
    128  (byte -128)
    255  (byte -1))
  (are [?n] (thrown? AssertionError (util/ubyte ?n))
    -1
    256))

(deftest divide-region
  (are [?start ?end ?step ?expected]
      (= (util/divide-region ?start ?end ?step) ?expected)
    1 10 1 [[1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7] [8 8] [9 9] [10 10]]
    1 10 2 [[1 2] [3 4] [5 6] [7 8] [9 10]]
    1 10 3 [[1 3] [4 6] [7 9] [10 10]]
    1 10 4 [[1 4] [5 8] [9 10]]
    1 10 5 [[1 5] [6 10]]
    1 10 6 [[1 6] [7 10]]
    1 10 7 [[1 7] [8 10]]
    1 10 8 [[1 8] [9 10]]
    1 10 9 [[1 9] [10 10]]
    1 10 10 [[1 10]]
    1 10 11 [[1 10]]))

(deftest divide-refs
  (are [?refs ?step ?expected]
      (= (util/divide-refs ?refs ?step) ?expected)
    [{:name "chr1" :len 10}] 4 [{:chr "chr1" :start 1 :end 4}
                                {:chr "chr1" :start 5 :end 8}
                                {:chr "chr1" :start 9 :end 10}]
    [{:name "chr1" :len 10}] 5 [{:chr "chr1" :start 1 :end 5}
                                {:chr "chr1" :start 6 :end 10}]
    [{:name "chr1" :len 10}] 10 [{:chr "chr1" :start 1 :end 10}]
    [{:name "chr1" :len 10}
     {:name "chr2" :len 5}] 6 [{:chr "chr1" :start 1 :end 6}
                               {:chr "chr1" :start 7 :end 10}
                               {:chr "chr2" :start 1 :end 5}]))

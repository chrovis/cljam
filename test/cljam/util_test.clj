(ns cljam.util-test
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

(deftest valid-rname?
  (are [?rname ?expected]
      (= ?expected (boolean (util/valid-rname? ?rname)))
    nil false
    "" false
    "c" true
    "chr1" true
    "*" false
    "=" false
    ":" true
    "chr:1" true
    "chr1:1-10" true))

(deftest valid-region?
  (are [?region-map ?expected]
      (= ?expected (boolean (util/valid-region? ?region-map)))
    nil false
    {} false
    {:chr "chr1"} false
    {:chr "chr1", :start 1} false
    {:chr "chr1", :start 1, :end 10} true
    {:chr "chr1", :start 100, :end 10} false
    {:chr "chr1", :start "1", :end 10} false
    {:chr "chr1", :start 1, :end "10"} false
    {:chr "chr1", :start 0, :end 10} false
    {:chr "chr1", :start 1, :end 0} false
    {:chr "", :start 100, :end 10} false
    {:chr " ", :start 100, :end 10} false))

(deftest parse-region
  (are [?region-str ?expected]
      (= ?expected (util/parse-region ?region-str))
    nil nil
    "*" nil
    "=" nil
    "c" {:chr "c"}
    "chr1" {:chr "chr1"}
    "chrUn" {:chr "chrUn"}
    "*chr1" nil
    "=chr1" nil
    "chr1*" {:chr "chr1*"}
    "chr1=" {:chr "chr1="}
    "chr1-" {:chr "chr1-"}
    "chr1:" {:chr "chr1"}
    "chr1:-" {:chr "chr1:-"}
    "!\"#$%&'()*+,-./0123456789;<=>?@[\\]^_`{|}~" {:chr "!\"#$%&'()*+,-./0123456789;<=>?@[\\]^_`{|}~"}
    "chr1:1" {:chr "chr1", :start 1}
    "chr1:1-" {:chr "chr1:1-"}
    "chr1:1-2" {:chr "chr1", :start 1, :end 2}
    "chr1:-2" {:chr "chr1", :end 2}
    "chr1:001-200" {:chr "chr1", :start 1, :end 200}
    "chr1:100-2" {:chr "chr1", :start 100, :end 2}
    "chr1:2:3-4" {:chr "chr1:2", :start 3, :end 4}
    "chr1:2-3:4-5" {:chr "chr1:2-3", :start 4, :end 5}
    "chr1:2-3:4-5:6-7" {:chr "chr1:2-3:4-5", :start 6, :end 7}))

(deftest parse-region-strict
  (are [?region-str ?expected]
      (= ?expected (util/parse-region-strict ?region-str))
    nil nil
    "*" nil
    "=" nil
    "c" nil
    "chr1" nil
    "chrUn" nil
    "*chr1" nil
    "=chr1" nil
    "chr1*" nil
    "chr1=" nil
    "chr1-" nil
    "chr1:" nil
    "chr1:-" nil
    "!\"#$%&'()*+,-./0123456789;<=>?@[\\]^_`{|}~" nil
    "chr1:1" nil
    "chr1:1-" nil
    "chr1:1-2" {:chr "chr1", :start 1, :end 2}
    "chr1:-2" nil
    "chr1:001-200" {:chr "chr1", :start 1, :end 200}
    "chr1:100-2" nil
    "chr1:2:3-4" {:chr "chr1:2", :start 3, :end 4}
    "chr1:2-3:4-5" {:chr "chr1:2-3", :start 4, :end 5}
    "chr1:2-3:4-5:6-7" {:chr "chr1:2-3:4-5", :start 6, :end 7}))

(deftest format-region
  (are [?region-map ?expected]
      (= ?expected (util/format-region ?region-map))
    nil nil
    {} nil
    {:chr "chr1"} "chr1"
    {:chr "chr1", :start 1} "chr1:1"
    {:chr "chr1", :start 1, :end 10} "chr1:1-10"
    {:chr "chr1", :start 10, :end 1} "chr1:10-1"
    {:chr "chr1", :end 10} "chr1"
    {:start 1} nil
    {:end 10} nil))

(deftest format-region-strict
  (are [?region-map ?expected]
      (= ?expected (util/format-region-strict ?region-map))
    nil nil
    {} nil
    {:chr "chr1"} nil
    {:chr "chr1", :start 1} nil
    {:chr "chr1", :start 1, :end 10} "chr1:1-10"
    {:chr "chr1", :start 10, :end 1} nil
    {:chr "chr1", :end 10} nil
    {:start 1} nil
    {:end 10} nil))

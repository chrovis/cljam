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

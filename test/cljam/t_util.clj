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

(deftest gen-vec
  (is (= (util/gen-vec 3) [nil nil nil]))
  (is (= (util/gen-vec 4 1) [1 1 1 1])))

(deftest str->int
  (is (= (util/str->int "123") 123))
  (is (= (util/str->int "-456") -456))
  (is (= (util/str->int "+789") 789))
  (is (instance? Integer (util/str->int "123")))
  (is (instance? Long (util/str->int "12345678901")))
  (is (= (util/str->int "12345678901") 12345678901)))

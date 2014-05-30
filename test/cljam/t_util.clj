(ns cljam.t-util
  "Tests for cljam.util."
  (:require [midje.sweet :refer :all]
            [cljam.util :as util]))

(tabular
 (fact "about ubyte"
   (util/ubyte ?n) => ?expected)
 ?n   ?expected
 0    (byte 0)
 127  (byte 127)
 128  (byte -128)
 255  (byte -1)
 -1   (throws AssertionError)
 256  (throws AssertionError))

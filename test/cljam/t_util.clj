(ns cljam.t-util
  (:use midje.sweet)
  (:require [cljam.util :as util]))

(fact
 (util/ubyte 0)   => (byte 0)
 (util/ubyte 127) => (byte 127)
 (util/ubyte 128) => (byte -128)
 (util/ubyte 255) => (byte -1)
 (util/ubyte -1)  => (throws AssertionError)
 (util/ubyte 256) => (throws AssertionError))

(fact
 (util/char-to-compressed-base-low \=) => (byte 0x0)
 (util/char-to-compressed-base-low \a) => (byte 0x1))

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
 (util/char->compressed-base-high \=) => (util/ubyte 0x0)

 (util/char->compressed-base-high \a) => (util/ubyte 0x10)
 (util/char->compressed-base-high \A) => (util/ubyte 0x10)

 (util/char->compressed-base-high \c) => (util/ubyte 0x20)
 (util/char->compressed-base-high \C) => (util/ubyte 0x20)

 (util/char->compressed-base-high \g) => (util/ubyte 0x40)
 (util/char->compressed-base-high \G) => (util/ubyte 0x40)

 (util/char->compressed-base-high \t) => (util/ubyte 0x80)
 (util/char->compressed-base-high \T) => (util/ubyte 0x80)

 (util/char->compressed-base-high \n) => (util/ubyte 0xf0)
 (util/char->compressed-base-high \N) => (util/ubyte 0xf0)
 (util/char->compressed-base-high \.) => (util/ubyte 0xf0)

 (util/char->compressed-base-high \m) => (util/ubyte 0x30)
 (util/char->compressed-base-high \M) => (util/ubyte 0x30)

 (util/char->compressed-base-high \r) => (util/ubyte 0x50)
 (util/char->compressed-base-high \R) => (util/ubyte 0x50)

 (util/char->compressed-base-high \s) => (util/ubyte 0x60)
 (util/char->compressed-base-high \S) => (util/ubyte 0x60)

 (util/char->compressed-base-high \v) => (util/ubyte 0x70)
 (util/char->compressed-base-high \V) => (util/ubyte 0x70)

 (util/char->compressed-base-high \w) => (util/ubyte 0x90)
 (util/char->compressed-base-high \W) => (util/ubyte 0x90)

 (util/char->compressed-base-high \y) => (util/ubyte 0xa0)
 (util/char->compressed-base-high \Y) => (util/ubyte 0xa0)

 (util/char->compressed-base-high \h) => (util/ubyte 0xb0)
 (util/char->compressed-base-high \H) => (util/ubyte 0xb0)

 (util/char->compressed-base-high \k) => (util/ubyte 0xc0)
 (util/char->compressed-base-high \K) => (util/ubyte 0xc0)

 (util/char->compressed-base-high \d) => (util/ubyte 0xd0)
 (util/char->compressed-base-high \D) => (util/ubyte 0xd0)

 (util/char->compressed-base-high \b) => (util/ubyte 0xe0)
 (util/char->compressed-base-high \B) => (util/ubyte 0xe0))

(fact
 (util/compressed-base->char-low (util/ubyte 0x0)) => \=
 (util/compressed-base->char-low (util/ubyte 0x1)) => \A
 (util/compressed-base->char-low (util/ubyte 0x2)) => \C
 (util/compressed-base->char-low (util/ubyte 0x4)) => \G
 (util/compressed-base->char-low (util/ubyte 0x8)) => \T
 (util/compressed-base->char-low (util/ubyte 0xf)) => \N
 (util/compressed-base->char-low (util/ubyte 0x3)) => \M
 (util/compressed-base->char-low (util/ubyte 0x5)) => \R
 (util/compressed-base->char-low (util/ubyte 0x6)) => \S
 (util/compressed-base->char-low (util/ubyte 0x7)) => \V
 (util/compressed-base->char-low (util/ubyte 0x9)) => \W
 (util/compressed-base->char-low (util/ubyte 0xa)) => \Y
 (util/compressed-base->char-low (util/ubyte 0xb)) => \H
 (util/compressed-base->char-low (util/ubyte 0xc)) => \K
 (util/compressed-base->char-low (util/ubyte 0xd)) => \D
 (util/compressed-base->char-low (util/ubyte 0xe)) => \B)

(fact
 (util/compressed-base->char-high (util/ubyte 0x0)) => \=
 (util/compressed-base->char-high (util/ubyte 0x10)) => \A
 (util/compressed-base->char-high (util/ubyte 0x20)) => \C
 (util/compressed-base->char-high (util/ubyte 0x40)) => \G
 (util/compressed-base->char-high (util/ubyte 0x80)) => \T
 (util/compressed-base->char-high (util/ubyte 0xf0)) => \N
 (util/compressed-base->char-high (util/ubyte 0x30)) => \M
 (util/compressed-base->char-high (util/ubyte 0x50)) => \R
 (util/compressed-base->char-high (util/ubyte 0x60)) => \S
 (util/compressed-base->char-high (util/ubyte 0x70)) => \V
 (util/compressed-base->char-high (util/ubyte 0x90)) => \W
 (util/compressed-base->char-high (util/ubyte 0xa0)) => \Y
 (util/compressed-base->char-high (util/ubyte 0xb0)) => \H
 (util/compressed-base->char-high (util/ubyte 0xc0)) => \K
 (util/compressed-base->char-high (util/ubyte 0xd0)) => \D
 (util/compressed-base->char-high (util/ubyte 0xe0)) => \B)

(ns cljam.util.t-sam-util
  "Tests for cljam.util.sam-util."
  (:require [midje.sweet :refer :all]
            [cljam.t-common :refer :all]
            [cljam.util :as util]
            [cljam.util.sam-util :as sam-util]))

(fact
  (sam-util/char->compressed-base-high \=) => (util/ubyte 0x0)

  (sam-util/char->compressed-base-high \a) => (util/ubyte 0x10)
  (sam-util/char->compressed-base-high \A) => (util/ubyte 0x10)

  (sam-util/char->compressed-base-high \c) => (util/ubyte 0x20)
  (sam-util/char->compressed-base-high \C) => (util/ubyte 0x20)

  (sam-util/char->compressed-base-high \g) => (util/ubyte 0x40)
  (sam-util/char->compressed-base-high \G) => (util/ubyte 0x40)

  (sam-util/char->compressed-base-high \t) => (util/ubyte 0x80)
  (sam-util/char->compressed-base-high \T) => (util/ubyte 0x80)

  (sam-util/char->compressed-base-high \n) => (util/ubyte 0xf0)
  (sam-util/char->compressed-base-high \N) => (util/ubyte 0xf0)
  (sam-util/char->compressed-base-high \.) => (util/ubyte 0xf0)

  (sam-util/char->compressed-base-high \m) => (util/ubyte 0x30)
  (sam-util/char->compressed-base-high \M) => (util/ubyte 0x30)

  (sam-util/char->compressed-base-high \r) => (util/ubyte 0x50)
  (sam-util/char->compressed-base-high \R) => (util/ubyte 0x50)

  (sam-util/char->compressed-base-high \s) => (util/ubyte 0x60)
  (sam-util/char->compressed-base-high \S) => (util/ubyte 0x60)

  (sam-util/char->compressed-base-high \v) => (util/ubyte 0x70)
  (sam-util/char->compressed-base-high \V) => (util/ubyte 0x70)

  (sam-util/char->compressed-base-high \w) => (util/ubyte 0x90)
  (sam-util/char->compressed-base-high \W) => (util/ubyte 0x90)

  (sam-util/char->compressed-base-high \y) => (util/ubyte 0xa0)
  (sam-util/char->compressed-base-high \Y) => (util/ubyte 0xa0)

  (sam-util/char->compressed-base-high \h) => (util/ubyte 0xb0)
  (sam-util/char->compressed-base-high \H) => (util/ubyte 0xb0)

  (sam-util/char->compressed-base-high \k) => (util/ubyte 0xc0)
  (sam-util/char->compressed-base-high \K) => (util/ubyte 0xc0)

  (sam-util/char->compressed-base-high \d) => (util/ubyte 0xd0)
  (sam-util/char->compressed-base-high \D) => (util/ubyte 0xd0)

  (sam-util/char->compressed-base-high \b) => (util/ubyte 0xe0)
  (sam-util/char->compressed-base-high \B) => (util/ubyte 0xe0))

(fact
  (sam-util/compressed-base->char-low (util/ubyte 0x0)) => \=
  (sam-util/compressed-base->char-low (util/ubyte 0x1)) => \A
  (sam-util/compressed-base->char-low (util/ubyte 0x2)) => \C
  (sam-util/compressed-base->char-low (util/ubyte 0x4)) => \G
  (sam-util/compressed-base->char-low (util/ubyte 0x8)) => \T
  (sam-util/compressed-base->char-low (util/ubyte 0xf)) => \N
  (sam-util/compressed-base->char-low (util/ubyte 0x3)) => \M
  (sam-util/compressed-base->char-low (util/ubyte 0x5)) => \R
  (sam-util/compressed-base->char-low (util/ubyte 0x6)) => \S
  (sam-util/compressed-base->char-low (util/ubyte 0x7)) => \V
  (sam-util/compressed-base->char-low (util/ubyte 0x9)) => \W
  (sam-util/compressed-base->char-low (util/ubyte 0xa)) => \Y
  (sam-util/compressed-base->char-low (util/ubyte 0xb)) => \H
  (sam-util/compressed-base->char-low (util/ubyte 0xc)) => \K
  (sam-util/compressed-base->char-low (util/ubyte 0xd)) => \D
  (sam-util/compressed-base->char-low (util/ubyte 0xe)) => \B)

(fact
  (sam-util/compressed-base->char-high (util/ubyte 0x0)) => \=
  (sam-util/compressed-base->char-high (util/ubyte 0x10)) => \A
  (sam-util/compressed-base->char-high (util/ubyte 0x20)) => \C
  (sam-util/compressed-base->char-high (util/ubyte 0x40)) => \G
  (sam-util/compressed-base->char-high (util/ubyte 0x80)) => \T
  (sam-util/compressed-base->char-high (util/ubyte 0xf0)) => \N
  (sam-util/compressed-base->char-high (util/ubyte 0x30)) => \M
  (sam-util/compressed-base->char-high (util/ubyte 0x50)) => \R
  (sam-util/compressed-base->char-high (util/ubyte 0x60)) => \S
  (sam-util/compressed-base->char-high (util/ubyte 0x70)) => \V
  (sam-util/compressed-base->char-high (util/ubyte 0x90)) => \W
  (sam-util/compressed-base->char-high (util/ubyte 0xa0)) => \Y
  (sam-util/compressed-base->char-high (util/ubyte 0xb0)) => \H
  (sam-util/compressed-base->char-high (util/ubyte 0xc0)) => \K
  (sam-util/compressed-base->char-high (util/ubyte 0xd0)) => \D
  (sam-util/compressed-base->char-high (util/ubyte 0xe0)) => \B)

;; Reference functions
;; -------------------

(def refs '({:name "ref", :len 45} {:name "ref2", :len 40}))

(fact "about make-refs"
  (sam-util/make-refs (:header test-sam)) => refs)

(fact "about ref-id"
  (sam-util/ref-id refs "ref") => 0
  (sam-util/ref-id refs "ref2") => 1
  (sam-util/ref-id refs "notfound") => nil?)

(fact "about ref-name"
  (sam-util/ref-name refs 0) => "ref"
  (sam-util/ref-name refs 1) => "ref2"
  (sam-util/ref-name refs 9) => nil?)

(fact "about ref-by-name"
  (sam-util/ref-by-name refs "ref") => {:name "ref", :len 45}
  (sam-util/ref-by-name refs "ref2") => {:name "ref2", :len 40}
  (sam-util/ref-by-name refs "notfound") => nil?)

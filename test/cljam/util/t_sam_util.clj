(ns cljam.util.t-sam-util
  "Tests for cljam.util.sam-util."
  (:require [midje.sweet :refer :all]
            [cljam.t-common :refer :all]
            [cljam.util :as util]
            [cljam.util.sam-util :as sam-util]
            [clojure.string :as cstr]))

(tabular
 (fact "about char->compressed-base-high"
   (sam-util/char->compressed-base-high ?base) => ?expected)
 ?base    ?expected
 \=       (util/ubyte 0x0)

 \a       (util/ubyte 0x10)
 \A       (util/ubyte 0x10)

 \c       (util/ubyte 0x20)
 \C       (util/ubyte 0x20)

 \g       (util/ubyte 0x40)
 \G       (util/ubyte 0x40)

 \t       (util/ubyte 0x80)
 \T       (util/ubyte 0x80)

 \n       (util/ubyte 0xf0)
 \N       (util/ubyte 0xf0)
 \.       (util/ubyte 0xf0)

 \m       (util/ubyte 0x30)
 \M       (util/ubyte 0x30)

 \r       (util/ubyte 0x50)
 \R       (util/ubyte 0x50)

 \s       (util/ubyte 0x60)
 \S       (util/ubyte 0x60)

 \v       (util/ubyte 0x70)
 \V       (util/ubyte 0x70)

 \w       (util/ubyte 0x90)
 \W       (util/ubyte 0x90)

 \y       (util/ubyte 0xa0)
 \Y       (util/ubyte 0xa0)

 \h       (util/ubyte 0xb0)
 \H       (util/ubyte 0xb0)

 \k       (util/ubyte 0xc0)
 \K       (util/ubyte 0xc0)

 \d       (util/ubyte 0xd0)
 \D       (util/ubyte 0xd0)

 \b       (util/ubyte 0xe0)
 \B       (util/ubyte 0xe0))

(tabular
 (fact "about compressed-base->char-low"
   (sam-util/compressed-base->char-low ?base) => ?expected)
 ?base              ?expected
 (util/ubyte 0x0)   \=
 (util/ubyte 0x1)   \A
 (util/ubyte 0x2)   \C
 (util/ubyte 0x4)   \G
 (util/ubyte 0x8)   \T
 (util/ubyte 0xf)   \N
 (util/ubyte 0x3)   \M
 (util/ubyte 0x5)   \R
 (util/ubyte 0x6)   \S
 (util/ubyte 0x7)   \V
 (util/ubyte 0x9)   \W
 (util/ubyte 0xa)   \Y
 (util/ubyte 0xb)   \H
 (util/ubyte 0xc)   \K
 (util/ubyte 0xd)   \D
 (util/ubyte 0xe)   \B)

(tabular
 (fact "about compressed-base->char-high"
   (sam-util/compressed-base->char-high ?base) => ?expected)
 ?base               ?expected
 (util/ubyte 0x0)    \=
 (util/ubyte 0x10)   \A
 (util/ubyte 0x20)   \C
 (util/ubyte 0x40)   \G
 (util/ubyte 0x80)   \T
 (util/ubyte 0xf0)   \N
 (util/ubyte 0x30)   \M
 (util/ubyte 0x50)   \R
 (util/ubyte 0x60)   \S
 (util/ubyte 0x70)   \V
 (util/ubyte 0x90)   \W
 (util/ubyte 0xa0)   \Y
 (util/ubyte 0xb0)   \H
 (util/ubyte 0xc0)   \K
 (util/ubyte 0xd0)   \D
 (util/ubyte 0xe0)   \B)

(def nibble-table "=ACMGRSVTWYHKDBN")

(tabular
 (fact
  "about compressed-bases->str"
  (let [ba (byte-array (mapv util/ubyte ?bases))]
    (cstr/join (sam-util/compressed-bases->chars ?length ba ?offset)) => ?expected
    (sam-util/compressed-bases->str ?length ba ?offset) => ?expected))
 ?length  ?bases                                    ?offset  ?expected
 1        [0x00]                                    0        "="
 2        [0x00]                                    0        "=="
 1        [0x10]                                    0        "A"
 2        [0x12]                                    0        "AC"
 4        [0x12 0x8F]                               0        "ACTN"
 1        [0x12 0x8F]                               1        "T"
 2        [0x12 0x8F]                               1        "TN"
 16       [0x01 0x23 0x45 0x67 0x89 0xAB 0xCD 0xEF] 0        nibble-table
 14       [0x01 0x23 0x45 0x67 0x89 0xAB 0xCD 0xEF] 1        (subs nibble-table 2)
 2        [0x01 0x23 0x45 0x67 0x89 0xAB 0xCD 0xEF] 7        "BN"
 512      (range 256)                               0        (->> (for [i nibble-table j nibble-table] [i j])
                                                                  (apply concat)
                                                                  cstr/join))

;; Reference functions
;; -------------------

(def refs '({:name "ref", :len 45} {:name "ref2", :len 40}))

(fact "about make-refs"
  (sam-util/make-refs (:header test-sam)) => refs)

(tabular
 (fact "about ref-id"
   (sam-util/ref-id refs ?name) => ?expected)
 ?name        ?expected
 "ref"        0
 "ref2"       1
 "notfound"   nil?)

(tabular
 (fact "about ref-name"
   (sam-util/ref-name refs ?id) => ?expected)
 ?id   ?expected
 0     "ref"
 1     "ref2"
 9     nil?)

(tabular
 (fact "about ref-by-name"
   (sam-util/ref-by-name refs ?name) => ?expected)
 ?name        ?expected
 "ref"        {:name "ref", :len 45}
 "ref2"       {:name "ref2", :len 40}
 "notfound"   nil?)

(tabular
 (facts "about flags"
   (sam-util/decode-flags ?flag) => ?set
   (sam-util/encode-flags ?set) => ?flag
   (sam-util/primary? ?flag) => ?primary
   (sam-util/primary? ?set) => ?primary)
 ?flag ?primary  ?set
 0     truthy    #{}
 1     truthy    #{:multiple}
 2     truthy    #{:properly-aligned}
 3     truthy    #{:multiple :properly-aligned}
 4     truthy    #{:unmapped}
 16    truthy    #{:reversed}
 83    truthy    #{:multiple :properly-aligned :reversed :first}
 163   truthy    #{:multiple :properly-aligned :next-reversed :last}
 99    truthy    #{:multiple :properly-aligned :next-reversed :first}
 147   truthy    #{:multiple :properly-aligned :reversed :last}
 121   truthy    #{:multiple :next-unmapped :reversed :next-reversed :first}
 181   truthy    #{:multiple :unmapped :reversed :next-reversed :last}
 77    truthy    #{:multiple :unmapped :next-unmapped :first}
 141   truthy    #{:multiple :unmapped :next-unmapped :last}
 256   falsey    #{:secondary}
 257   falsey    #{:multiple :secondary}
 2048  falsey    #{:supplementary}
 2049  falsey    #{:multiple :supplementary}
 2304  falsey    #{:secondary :supplementary}
 0x900 falsey    #{:secondary :supplementary})

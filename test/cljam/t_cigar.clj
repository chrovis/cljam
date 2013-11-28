(ns cljam.t-cigar
  (:require [midje.sweet :refer :all]
            [cljam.cigar :as cgr]))

(fact "about parse"
  (cgr/parse  "1S2I6M1P11I") => '([1 \S] [2 \I] [6 \M] [1 \P] [11 \I]))

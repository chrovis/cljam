(ns cljam.io.sam.util.flag-test
  (:require [clojure.test :refer [deftest are testing]]
            [cljam.io.sam.util.flag :as flag]))

(deftest flags
  (are [?flag ?primary ?set] (and (= (flag/decode ?flag) ?set)
                                  (= (flag/encode ?set) ?flag)
                                  (= (flag/primary? ?flag) ?primary))
    0     true  #{}
    1     true  #{:multiple}
    2     true  #{:properly-aligned}
    3     true  #{:multiple :properly-aligned}
    4     true  #{:unmapped}
    16    true  #{:reversed}
    83    true  #{:multiple :properly-aligned :reversed :first}
    163   true  #{:multiple :properly-aligned :next-reversed :last}
    99    true  #{:multiple :properly-aligned :next-reversed :first}
    147   true  #{:multiple :properly-aligned :reversed :last}
    121   true  #{:multiple :next-unmapped :reversed :next-reversed :first}
    181   true  #{:multiple :unmapped :reversed :next-reversed :last}
    77    true  #{:multiple :unmapped :next-unmapped :first}
    141   true  #{:multiple :unmapped :next-unmapped :last}
    256   false #{:secondary}
    257   false #{:multiple :secondary}
    2048  false #{:supplementary}
    2049  false #{:multiple :supplementary}
    2304  false #{:secondary :supplementary}
    0x900 false #{:secondary :supplementary}))

(deftest flag-tests
  (testing "multiple?"
    (are [?flag]
         (= (flag/multiple? ?flag) false)
      0 4)
    (are [?flag]
         (= (flag/multiple? ?flag) true)
      67 79 83 99 147 403 355))
  (testing "properly-aligned?"
    (are [?flag]
         (= (flag/properly-aligned? ?flag) false)
      0 4)
    (are [?flag]
         (= (flag/properly-aligned? ?flag) true)
      67 79 83 99 147 403 355))
  (testing "unmapped?"
    (are [?flag]
         (= (flag/unmapped? ?flag) false)
      0 67 83 99 147 403 355)
    (are [?flag]
         (= (flag/unmapped? ?flag) true)
      4 79))
  (testing "both-unmapped?"
    (are [?flag]
         (= (flag/both-unmapped? ?flag) false)
      0 4 67 83 99 147 403 355)
    (are [?flag]
         (= (flag/both-unmapped? ?flag) true)
      79))
  (testing "reversed?"
    (are [?flag]
         (= (flag/reversed? ?flag) false)
      0 4 67 79 99 355)
    (are [?flag]
         (= (flag/reversed? ?flag) true)
      16 83 147 403))
  (testing "r1?"
    (are [?flag]
         (= (flag/r1? ?flag) false)
      0 4 16 147 403)
    (are [?flag]
         (= (flag/r1? ?flag) true)
      67 79 83 99 355))
  (testing "r2?"
    (are [?flag]
         (= (flag/r2? ?flag) false)
      0 4 16 67 79 83 99 355)
    (are [?flag]
         (= (flag/r2? ?flag) true)
      147 403))
  (testing "r1r2"
    (are [?flag]
         (= (flag/r1r2 ?flag) 0)
      0 4 16)
    (are [?flag]
         (= (flag/r1r2 ?flag) 1)
      67 79 83 99 355)
    (are [?flag]
         (= (flag/r1r2 ?flag) 2)
      147 403))
  (testing "secondary?"
    (are [?flag]
         (= (flag/secondary? ?flag) false)
      0 4 16 67 79 83 99 147)
    (are [?flag]
         (= (flag/secondary? ?flag) true)
      403 355)))

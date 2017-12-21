(ns cljam.io.sam.util.flag-test
  (:require [clojure.test :refer :all]
            [cljam.io.sam.util.flag :as flag]))

(deftest flags
  (are [?flag ?primary ?set] (and (= (flag/decode ?flag) ?set)
                                  (= (flag/encode ?set) ?flag)
                                  (= (flag/primary? ?flag) ?primary)
                                  (= (flag/primary? ?set) ?primary))
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

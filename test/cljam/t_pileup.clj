(ns cljam.t-pileup
  (:use midje.sweet)
  (:require (cljam sam
                   [pileup :as pileup]))
  (:import (cljam.sam Sam SamHeader SamAlignment)))

(def test-sam
  (Sam. [(assoc (SamHeader.) :HD {:VN "1.4",  :SO "coordinate"})
         (assoc (SamHeader.) :SQ {:SN "ref",  :LN "45"})
         (assoc (SamHeader.) :SQ {:SN "ref2", :LN "40"})]
        [(SamAlignment. "r001" 163 "ref"  7  30 "8M4I4M1D3M"         "=" 37 39  "TTAGATAAAGAGGATACTG"        "*"                          [{:XX {:type "B", :value "S,12561,2,20,112"}}])
         (SamAlignment. "r002" 0   "ref"  9  30 "1S2I6M1P1I1P1I4M2I" "*" 0  0   "AAAAGATAAGGGATAAA"          "*"                          [])
         (SamAlignment. "r003" 0   "ref"  9  30 "5H6M"               "*" 0  0   "AGCTAA"                     "*"                          [])
         (SamAlignment. "x1"   0   "ref2" 1  30 "20M"                "*" 0  0   "AGGTTTTATAAAACAAATAA"       "????????????????????"       [])
         (SamAlignment. "x2"   0   "ref2" 2  30 "21M"                "*" 0  0   "GGTTTTATAAAACAAATAATT"      "?????????????????????"      [])
         (SamAlignment. "x3"   0   "ref2" 6  30 "9M4I13M"            "*" 0  0   "TTATAAAACAAATAATTAAGTCTACA" "??????????????????????????" [])]))

(fact "about pileup"
  (pileup/pileup test-sam) => '({:rname "ref",  :pos 7,  :n 1}
                                {:rname "ref",  :pos 8,  :n 1}
                                {:rname "ref",  :pos 9,  :n 3}
                                {:rname "ref",  :pos 10, :n 3}
                                {:rname "ref",  :pos 11, :n 3}
                                {:rname "ref",  :pos 12, :n 3}
                                {:rname "ref",  :pos 13, :n 3}
                                {:rname "ref",  :pos 14, :n 3}
                                {:rname "ref",  :pos 15, :n 2}
                                {:rname "ref",  :pos 16, :n 2}
                                {:rname "ref",  :pos 17, :n 2}
                                {:rname "ref",  :pos 18, :n 2}
                                {:rname "ref",  :pos 19, :n 1}
                                {:rname "ref",  :pos 20, :n 1}
                                {:rname "ref",  :pos 21, :n 1}
                                {:rname "ref",  :pos 22, :n 1}
                                {:rname "ref2", :pos 1 , :n 1}
                                {:rname "ref2", :pos 2 , :n 2}
                                {:rname "ref2", :pos 3 , :n 2}
                                {:rname "ref2", :pos 4 , :n 2}
                                {:rname "ref2", :pos 5 , :n 2}
                                {:rname "ref2", :pos 6 , :n 3}
                                {:rname "ref2", :pos 7 , :n 3}
                                {:rname "ref2", :pos 8 , :n 3}
                                {:rname "ref2", :pos 9 , :n 3}
                                {:rname "ref2", :pos 10, :n 3}
                                {:rname "ref2", :pos 11, :n 3}
                                {:rname "ref2", :pos 12, :n 3}
                                {:rname "ref2", :pos 13, :n 3}
                                {:rname "ref2", :pos 14, :n 3}
                                {:rname "ref2", :pos 15, :n 3}
                                {:rname "ref2", :pos 16, :n 3}
                                {:rname "ref2", :pos 17, :n 3}
                                {:rname "ref2", :pos 18, :n 3}
                                {:rname "ref2", :pos 19, :n 3}
                                {:rname "ref2", :pos 20, :n 3}
                                {:rname "ref2", :pos 21, :n 2}
                                {:rname "ref2", :pos 22, :n 2}
                                {:rname "ref2", :pos 23, :n 1}
                                {:rname "ref2", :pos 24, :n 1}
                                {:rname "ref2", :pos 25, :n 1}
                                {:rname "ref2", :pos 26, :n 1}
                                {:rname "ref2", :pos 27, :n 1}))

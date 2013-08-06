(ns cljam.t-pileup
  (:use midje.sweet)
  (:require (cljam sam
                   [pileup :as pileup]))
  (:import (cljam.sam Sam SamHeader SamAlignment)))

(def test-sam
  {:header {:HD {:VN "1.4",  :SO "coordinate"}
            :SQ '({:SN "ref",  :LN "45"} {:SN "ref2", :LN "40"})}
   :alignments
   '({:qname "r001", :flag 163, :rname "ref" , :pos 7, :mapq 30, :cigar "8M4I4M1D3M"        , :rnext "=", :pnext 37, :tlen 39, :seq  "TTAGATAAAGAGGATACTG"       , :qual "*"                         , :options [{:XX {:type "B", :value "S,12561,2,20,112"}}]}
     {:qname "r002", :flag 0  , :rname "ref" , :pos 9, :mapq 30, :cigar "1S2I6M1P1I1P1I4M2I", :rnext "*", :pnext 0 , :tlen 0 , :seq  "AAAAGATAAGGGATAAA"         , :qual "*"                         , :options []}
     {:qname "r003", :flag 0  , :rname "ref" , :pos 9, :mapq 30, :cigar "5H6M"              , :rnext "*", :pnext 0 , :tlen 0 , :seq  "AGCTAA"                    , :qual "*"                         , :options []}
     {:qname "x1"  , :flag 0  , :rname "ref2", :pos 1, :mapq 30, :cigar "20M"               , :rnext "*", :pnext 0 , :tlen 0 , :seq  "AGGTTTTATAAAACAAATAA"      , :qual "????????????????????"      , :options []}
     {:qname "x2"  , :flag 0  , :rname "ref2", :pos 2, :mapq 30, :cigar "21M"               , :rnext "*", :pnext 0 , :tlen 0 , :seq  "GGTTTTATAAAACAAATAATT"     , :qual "?????????????????????"     , :options []}
     {:qname "x3"  , :flag 0  , :rname "ref2", :pos 6, :mapq 30, :cigar "9M4I13M"           , :rnext "*", :pnext 0 , :tlen 0 , :seq  "TTATAAAACAAATAATTAAGTCTACA", :qual "??????????????????????????", :options []})})

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

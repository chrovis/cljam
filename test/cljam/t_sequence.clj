(ns cljam.t-sequence
  (:require [midje.sweet :refer :all]
            [cljam.sequence :as cseq]))

(fact "about parse"
  (cseq/parse "TTAGATAAAGAGGATACTG" "8M4I4M1D3M") => '({:n 8, :op \M, :seq [\T \T \A \G \A \T \A \A]}
                                                       {:n 4, :op \I, :seq [\A \G \A \G]}
                                                       {:n 4, :op \M, :seq [\G \A \T \A]}
                                                       {:n 1, :op \D, :seq [\*]}
                                                       {:n 3, :op \M, :seq [\C \T \G]})

  (cseq/parse "AAAAGATAAGGGATAAA" "1S2I6M1P1I1P1I4M2I") => '({:n 1, :op \S, :seq [\A]}
                                                             {:n 2, :op \I, :seq [\A \A]}
                                                             {:n 6, :op \M, :seq [\A \G \A \T \A \A]}
                                                             {:n 1, :op \P, :seq nil}
                                                             {:n 1, :op \I, :seq [\G]}
                                                             {:n 1, :op \P, :seq nil}
                                                             {:n 1, :op \I, :seq [\G]}
                                                             {:n 4, :op \M, :seq [\G \A \T \A]}
                                                             {:n 2, :op \I, :seq [\A \A]})

  (cseq/parse "AGCTAA" "5H6M") => '({:n 5, :op \H, :seq nil}
                                    {:n 6, :op \M, :seq [\A \G \C \T \A \A]})

  (cseq/parse "ATAGCTCTCAGC" "6M14N1I5M") => '({:n 6, :op \M, :seq [\A \T \A \G \C \T]}
                                               {:n 14, :op \N, :seq [\> \> \> \> \> \> \> \> \> \> \> \> \> \>]}
                                               {:n 1, :op \I, :seq [\C]}
                                               {:n 5, :op \M, :seq [\T \C \A \G \C]}))

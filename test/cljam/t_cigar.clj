(ns cljam.t-cigar
  (:require [midje.sweet :refer :all]
            [cljam.cigar :as cgr]))

(fact "about parse-seq"
  (cgr/parse-seq "8M4I4M1D3M" "TTAGATAAAGAGGATACTG") => '({:n 8, :op \M, :seq [\T \T \A \G \A \T \A \A]}
                                                          {:n 4, :op \I, :seq [\A \G \A \G]}
                                                          {:n 4, :op \M, :seq [\G \A \T \A]}
                                                          {:n 1, :op \D, :seq [\*]}
                                                          {:n 3, :op \M, :seq [\C \T \G]})
  (cgr/parse-seq "1S2I6M1P1I1P1I4M2I" "AAAAGATAAGGGATAAA") => '({:n 1, :op \S, :seq [\A]}
                                                                {:n 2, :op \I, :seq [\A \A]}
                                                                {:n 6, :op \M, :seq [\A \G \A \T \A \A]}
                                                                {:n 1, :op \P, :seq nil}
                                                                {:n 1, :op \I, :seq [\G]}
                                                                {:n 1, :op \P, :seq nil}
                                                                {:n 1, :op \I, :seq [\G]}
                                                                {:n 4, :op \M, :seq [\G \A \T \A]}
                                                                {:n 2, :op \I, :seq [\A \A]})
  (cgr/parse-seq "5H6M" "AGCTAA") => '({:n 5, :op \H, :seq nil}
                                       {:n 6, :op \M, :seq [\A \G \C \T \A \A]})
  (cgr/parse-seq "6M14N1I5M" "ATAGCTCTCAGC") => '({:n 6, :op \M, :seq [\A \T \A \G \C \T]}
                                                  {:n 14, :op \N, :seq [\> \> \> \> \> \> \> \> \> \> \> \> \> \>]}
                                                  {:n 1, :op \I, :seq [\C]}
                                                  {:n 5, :op \M, :seq [\T \C \A \G \C]}))

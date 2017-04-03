(ns cljam.t-cigar
  #?(:clj (:require [midje.sweet :refer :all]
                    [cljam.cigar :as cgr])
     :cljs (:require [cljs.test :refer-macros [deftest is testing]]
                     [cljam.cigar :as cgr])))

#?(:clj
   (fact "about parse"
         (cgr/parse  "1S2I6M1P11I") => '([1 \S] [2 \I] [6 \M] [1 \P] [11 \I]))
   :cljs
   (deftest test-cigar-parse
     (testing "about parse"
       (is (= (cgr/parse  "1S2I6M1P11I")
              '([1 \S] [2 \I] [6 \M] [1 \P] [11 \I]))))))

#?(:clj
   (tabular
    (fact
     "cigar to index"
     (map (fn [[op x :as xs]] (if (= op :m) x xs)) (cgr/to-index* ?cigar)) => ?index)
    ?cigar               ?index
    "4M"                 [0 1 2 3]
    "1M3I"               [[:i 0 [1 2 3]]]
    "1M3D"               [[:d 0 [1 2 3]] \* \* \*]
    "2M3I"               [0 [:i 1 [2 3 4]]]
    "4D5M"               [\* \* \* \* 0 1 2 3 4] ;; ^]* * * * A T G C A$
    "4D5I"               [\* \* \* [:i \* [0 1 2 3 4]]] ;; TODO: ^]* * * *+5TGCA=$
    "4S4M"               [4 5 6 7]
    "4H4M"               [0 1 2 3]
    "1M3I1M"             [[:i 0 [1 2 3]] 4]
    "2M3I1M"             [0 [:i 1 [2 3 4]] 5]
    "4I2D5M"             [\* \* 4 5 6 7 8] ;; ^]* * A T G C A$
    "1M4D5I"             [[:d 0 [1 2 3 4]] \* \* \* [:i \* [1 2 3 4 5]]] ;; TODO: ^]* * * *+5TGCA=$
    "1M3D3M"             [[:d 0 [1 2 3]] \* \* \* 1 2 3]
    "1M2D1I2M"           [[:d 0 [1 2]] \* [:i \* [1]] 2 3] ;; TODO: ^]A-2NN * *+1G G C$
    "1M2N1I2M"           [0 \> [:i \> [1]] 2 3] ;; TODO: ^]A > >+1G G C$
    "1M1I2D2M"           [[:i 0 [1]] \* \* 2 3] ;; ^]A+1T * * G C$
    "1M1I2N2M"           [[:i 0 [1]] \> \> 2 3] ;; ^]A+1T > > G C$
    "1M4I2D5M"           [[:i 0 [1 2 3 4]] \* \* 5 6 7 8 9] ;; ^]A+4TGCA * * C A T G C$
    "1S4I2D5M"           [\* \* 5 6 7 8 9] ;; ^]* * T G C A T$
    "3M2P2I3M"           [0 1 [:i 2 [3 4]] 5 6 7] ;; ^]A T G+2CA T G C$
    "1P4I1D5M"           [\* 4 5 6 7 8] ;; ^]* A T G C A$
    "5M1D3M4S"           [0 1 2 3 [:d 4 [5]] \* 5 6 7] ;; ^]A T G C A-1N * T G C$
    "5M1N3M4S"           [0 1 2 3 4 \> 5 6 7] ;; ^]A T G C A > T G C$
    "3H2S3M1D2S4H"       [2 3 [:d 4 [3]] \*] ;; ^]G C A-1N \*$
    "1M1P1I1P1I2M"       [[:i 0 [1 2]] 3 4] ;; ^]A+2TG C A$
    "2M1I1D2I2D1M"       [0 [:i 1 [2]] [:i \* [3 4]] \* \* 5] ;; TODO: ^]A T+1G *+2AT * * T$
    "1S2I6M1P1I1P1I4M2I" [3 4 5 6 7 [:i 8 [9 10]] 11 12 13 [:i 14 [15 16]]] ;; ^]C A T G C A+2TG C A T G+2CA$
    "6M14D1I5M"          [0 1 2 3 4 [:d 5 [6 7 8 9 10 11 12 13 14 15 16 17 18 19]] \* \* \* \* \* \* \* \* \* \* \* \* \* [:i \* [6]] 7 8 9 10 11]  ;; ^]A T G C A T-14NNNNNNNNNNNNNN * * * * * * * * * * * * * *+1C C A T G C$
    "6M14N1I5M"          [0 1 2 3 4 5 \> \> \> \> \> \> \> \> \> \> \> \> \> [:i \> [6]] 7 8 9 10 11] ;; ^]A T G C A T > > > > > > > > > > > > > >+1C C A T G C$
    "1S2M2D3M1D4M3D2M3D3M3I4M3D2M3S" [1 [:d 2 [2 3]] \* \* 3 4 [:d 5 [7]] \* 6 7  8 [:d 9 [12 13 14]] \* \* \* 10 [:d 11 [17 18 19]] \* \* \* 12 13 [:i 14 [15 16 17]] 18 19 20 [:d 21 [27 28 29]] \* \* \* 22 23])
   :cljs
   (deftest test-cigar-to-index
     (testing "cigar to index"
       (let [to-index (fn [c] (map (fn [[op x :as xs]] (if (= op :m) x xs)) (cgr/to-index* c)))
             test-to-index (fn [src dst] (is (= (to-index src) dst)))]
         (test-to-index "4M"                 [0 1 2 3])
         (test-to-index "1M3I"               [[:i 0 [1 2 3]]])
         (test-to-index "1M3D"               [[:d 0 [1 2 3]] \* \* \*])
         (test-to-index "2M3I"               [0 [:i 1 [2 3 4]]])
         (test-to-index "4D5M"               [\* \* \* \* 0 1 2 3 4])
         (test-to-index "4D5I"               [\* \* \* [:i \* [0 1 2 3 4]]])
         (test-to-index "4S4M"               [4 5 6 7])
         (test-to-index "4H4M"               [0 1 2 3])
         (test-to-index "1M3I1M"             [[:i 0 [1 2 3]] 4])
         (test-to-index "2M3I1M"             [0 [:i 1 [2 3 4]] 5])
         (test-to-index "4I2D5M"             [\* \* 4 5 6 7 8])
         (test-to-index "1M4D5I"             [[:d 0 [1 2 3 4]] \* \* \* [:i \* [1 2 3 4 5]]])
         (test-to-index "1M3D3M"             [[:d 0 [1 2 3]] \* \* \* 1 2 3])
         (test-to-index "1M2D1I2M"           [[:d 0 [1 2]] \* [:i \* [1]] 2 3])
         (test-to-index "1M2N1I2M"           [0 \> [:i \> [1]] 2 3])
         (test-to-index "1M1I2D2M"           [[:i 0 [1]] \* \* 2 3])
         (test-to-index "1M1I2N2M"           [[:i 0 [1]] \> \> 2 3])
         (test-to-index "1M4I2D5M"           [[:i 0 [1 2 3 4]] \* \* 5 6 7 8 9])
         (test-to-index "1S4I2D5M"           [\* \* 5 6 7 8 9])
         (test-to-index "3M2P2I3M"           [0 1 [:i 2 [3 4]] 5 6 7])
         (test-to-index "1P4I1D5M"           [\* 4 5 6 7 8])
         (test-to-index "5M1D3M4S"           [0 1 2 3 [:d 4 [5]] \* 5 6 7])
         (test-to-index "5M1N3M4S"           [0 1 2 3 4 \> 5 6 7])
         (test-to-index "3H2S3M1D2S4H"       [2 3 [:d 4 [3]] \*])
         (test-to-index "1M1P1I1P1I2M"       [[:i 0 [1 2]] 3 4])
         (test-to-index "2M1I1D2I2D1M"       [0 [:i 1 [2]] [:i \* [3 4]] \* \* 5])
         (test-to-index "1S2I6M1P1I1P1I4M2I" [3 4 5 6 7 [:i 8 [9 10]] 11 12 13 [:i 14 [15 16]]])
         (test-to-index "6M14D1I5M"          [0 1 2 3 4 [:d 5 [6 7 8 9 10 11 12 13 14 15 16 17 18 19]] \* \* \* \* \* \* \* \* \* \* \* \* \* [:i \* [6]] 7 8 9 10 11])
         (test-to-index "6M14N1I5M"          [0 1 2 3 4 5 \> \> \> \> \> \> \> \> \> \> \> \> \> [:i \> [6]] 7 8 9 10 11])
         (test-to-index "1S2M2D3M1D4M3D2M3D3M3I4M3D2M3S" [1 [:d 2 [2 3]] \* \* 3 4 [:d 5 [7]] \* 6 7  8 [:d 9 [12 13 14]] \* \* \* 10 [:d 11 [17 18 19]] \* \* \* 12 13 [:i 14 [15 16 17]] 18 19 20 [:d 21 [27 28 29]] \* \* \* 22 23])))))

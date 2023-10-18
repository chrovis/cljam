(ns cljam.io.sam.util.cigar-test
  (:require [clojure.test :refer [deftest is are]]
            [cljam.io.sam.util.cigar :as cigar])
  (:import [java.nio ByteBuffer ByteOrder]))

(defn- ints->bytes ^bytes [ints']
  (let [b (ByteBuffer/allocate (* 4 (count ints')))
        _ (.order b ByteOrder/LITTLE_ENDIAN)
        ib (.asIntBuffer b)]
    (doseq [i ints']
      (.put ib (unchecked-int i)))
    (.array b)))

(deftest count-ref-bytes
  (are [?cigar ?expected]
       (= ?expected
          (-> ?cigar
              cigar/encode-cigar
              ints->bytes
              cigar/count-ref-bytes))
    "10M" 10
    "10S" 0
    "10M10I" 10
    "10M10D" 20
    "134217727M" 134217727 ;; 2^27 - 1, 0x07FFFFFF
    "134217728M" 134217728 ;; 2^27    , 0x08000000
    "268435455M" 268435455 ;; 2^28 - 1, 0x0FFFFFFF
    ))

(deftest decode-cigar-and-ref-length
  (are [?cigar ?ref-length]
       (= [?cigar ?ref-length]
          (-> ?cigar
              cigar/encode-cigar
              ints->bytes
              cigar/decode-cigar-and-ref-length))
    "10M" 10
    "10S" 0
    "10M10I" 10
    "10M10D" 20
    "134217727M" 134217727 ;; 2^27 - 1, 0x07FFFFFF
    "134217728M" 134217728 ;; 2^27    , 0x08000000
    "268435455M" 268435455 ;; 2^28 - 1, 0x0FFFFFFF
    ))

(deftest about-parse
  (is (= (cigar/parse  "1S2I6M1P11I") '([1 \S] [2 \I] [6 \M] [1 \P] [11 \I]))))

(deftest cigar-to-index
  (are [?cigar ?index] (= ?index (map (fn [[op x :as xs]] (if (= op :m) x xs)) (cigar/to-index* ?cigar)))
    "4M"                 [0 1 2 3]
    "1M3I"               [[:i 0 [1 4]]]
    "1M3D"               [[:d 0 3] \* \* \*]
    "2M3I"               [0 [:i 1 [2 5]]]
    "4D5M"               [\* \* \* \* 0 1 2 3 4] ;; ^]* * * * A T G C A$
    "4D5I"               [\* \* \* [:i \* [0 5]]] ;; TODO: ^]* * * *+5TGCA=$
    "4S4M"               [4 5 6 7]
    "4H4M"               [0 1 2 3]
    "1M3I1M"             [[:i 0 [1 4]] 4]
    "2M3I1M"             [0 [:i 1 [2 5]] 5]
    "4I2D5M"             [\* \* 4 5 6 7 8] ;; ^]* * A T G C A$
    "1M4D5I"             [[:d 0 4] \* \* \* [:i \* [1 6]]] ;; TODO: ^]* * * *+5TGCA=$
    "1M3D3M"             [[:d 0 3] \* \* \* 1 2 3]
    "1M2D1I2M"           [[:d 0 2] \* [:i \* [1 2]] 2 3] ;; TODO: ^]A-2NN * *+1G G C$
    "1M2N1I2M"           [0 \> [:i \> [1 2]] 2 3] ;; TODO: ^]A > >+1G G C$
    "1M1I2D2M"           [[:i 0 [1 2]] \* \* 2 3] ;; ^]A+1T * * G C$
    "1M1I2N2M"           [[:i 0 [1 2]] \> \> 2 3] ;; ^]A+1T > > G C$
    "1M4I2D5M"           [[:i 0 [1 5]] \* \* 5 6 7 8 9] ;; ^]A+4TGCA * * C A T G C$
    "1S4I2D5M"           [\* \* 5 6 7 8 9] ;; ^]* * T G C A T$
    "3M2P2I3M"           [0 1 [:i 2 [3 5]] 5 6 7] ;; ^]A T G+2CA T G C$
    "1P4I1D5M"           [\* 4 5 6 7 8] ;; ^]* A T G C A$
    "5M1D3M4S"           [0 1 2 3 [:d 4 1] \* 5 6 7] ;; ^]A T G C A-1N * T G C$
    "5M1N3M4S"           [0 1 2 3 4 \> 5 6 7] ;; ^]A T G C A > T G C$
    "3H2S3M1D2S4H"       [2 3 [:d 4 1] \*] ;; ^]G C A-1N \*$
    "1M1P1I1P1I2M"       [[:i 0 [1 3]] 3 4] ;; ^]A+2TG C A$
    "2M1I1D2I2D1M"       [0 [:i 1 [2 3]] [:i \* [3 5]] \* \* 5] ;; TODO: ^]A T+1G *+2AT * * T$
    "1S2I6M1P1I1P1I4M2I" [3 4 5 6 7 [:i 8 [9 11]] 11 12 13 [:i 14 [15 17]]] ;; ^]C A T G C A+2TG C A T G+2CA$
    "6M14D1I5M"          [0 1 2 3 4 [:d 5 14] \* \* \* \* \* \* \* \* \* \* \* \* \* [:i \* [6 7]] 7 8 9 10 11]  ;; ^]A T G C A T-14NNNNNNNNNNNNNN * * * * * * * * * * * * * *+1C C A T G C$
    "6M14N1I5M"          [0 1 2 3 4 5 \> \> \> \> \> \> \> \> \> \> \> \> \> [:i \> [6 7]] 7 8 9 10 11] ;; ^]A T G C A T > > > > > > > > > > > > > >+1C C A T G C$
    "1S2M2D3M1D4M3D2M3D3M3I4M3D2M3S" [1 [:d 2 2] \* \* 3 4 [:d 5 1] \* 6 7  8 [:d 9 3] \* \* \* 10 [:d 11 3] \* \* \* 12 13 [:i 14 [15 18]] 18 19 20 [:d 21 3] \* \* \* 22 23]))

(deftest placeholder?
  (are [?cigar ?expected]
       (= ?expected
          (-> ?cigar
              cigar/encode-cigar
              ints->bytes
              cigar/placeholder?))
    "1S2N" true
    "123456789S987654321N" true
    "100000S200000N1000000S" false
    "1S" false
    "22S33333M" false
    "444444444H5555555N" false))

(deftest ->plaeholder
  (are [?cigar ?expected]
       (= ?expected (cigar/->placeholder ?cigar))
    "12H30S140M5I32M" [3316 2755] ;; 207S172N
    "10M20I100M200I1000M2000I" [53284 17763] ;; 3330S1110N
    "1M2I4D8N16S32H64P128=256X" [6452 6355] ;; 403S397N
    ))

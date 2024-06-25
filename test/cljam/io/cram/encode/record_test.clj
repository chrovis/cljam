(ns cljam.io.cram.encode.record-test
  (:require [cljam.io.cram.data-series :as ds]
            [cljam.io.cram.encode.record :as record]
            [cljam.io.cram.seq-resolver.protocol :as resolver]
            [cljam.io.sequence :as cseq]
            [cljam.test-common :as common]
            [clojure.test :refer [are deftest is testing]]
            [clojure.walk :as walk]))

(def ^:private test-seq-resolver
  (let [seqs (with-open [r (cseq/reader common/test-fa-file)]
               (into {} (map (juxt :name :sequence)) (cseq/read-all-sequences r)))]
    (reify resolver/ISeqResolver
      (resolve-sequence [_ chr]
        (.getBytes ^bytes (get seqs chr)))
      (resolve-sequence [_ chr start end]
        (let [s (get seqs chr)]
          (.getBytes (subs s (dec (long start)) end)))))))

(def subst-mat
  {\A {\T 0, \G 1, \C 2, \N 3}
   \T {\A 0, \G 1, \C 2, \N 3}
   \G {\A 0, \T 1, \C 2, \N 3}
   \C {\A 0, \T 1, \G 2, \N 3}
   \N {\A 0, \T 1, \G 2, \C 3}})

(deftest calculate-read-features&end
  (are [?record ?expected]
       (= ?expected
          (walk/prewalk
           #(if (instance? (Class/forName "[B") %) (vec %) %)
           (#'record/calculate-read-features&end test-seq-resolver subst-mat ?record)))
    {:rname "ref", :pos 2, :seq "GCATG", :qual "ABCDE", :cigar "5M"}
    [[]
     6]

    {:rname "ref", :pos 2, :seq "GCCTG", :qual "ABCDE", :cigar "5M"}
    [[{:code :subst, :pos 3, :subst 2}]
     6]

    {:rname "ref", :pos 2, :seq "GCRTG", :qual "ABCDE", :cigar "5M"}
    [[{:code :read-base, :pos 3, :base (int \R), :qual (- (int \C) 33)}]
     6]

    {:rname "ref", :pos 2, :seq "GCCAA", :qual "ABCDE", :cigar "2M2I1M"}
    [[{:code :insertion, :pos 3, :bases (mapv int "CA")}]
     4]

    {:rname "ref", :pos 2, :seq "GTCAA", :qual "ABCDE", :cigar "2M2I1M"}
    [[{:code :subst, :pos 2, :subst 1}
      {:code :insertion, :pos 3, :bases (mapv int "CA")}]
     4]

    {:rname "ref", :pos 2, :seq "GCGTT", :qual "ABCDE", :cigar "2M2D3M"}
    [[{:code :deletion, :pos 3, :len 2}]
     8]

    {:rname "ref", :pos 2, :seq "GCCTT", :qual "ABCDE", :cigar "2M2D3M"}
    [[{:code :deletion, :pos 3, :len 2}
      {:code :subst, :pos 3, :subst 2}]
     8]

    {:rname "ref", :pos 2, :seq "GCATT", :qual "ABCDE", :cigar "3M2S"}
    [[{:code :softclip, :pos 4, :bases (mapv int "TT")}]
     4]

    {:rname "ref", :pos 2, :seq "TTGCA", :qual "ABCDE", :cigar "2S3M"}
    [[{:code :softclip, :pos 1, :bases (mapv int "TT")}]
     4]))

(deftest encode-slice-records-test
  (testing "mapped reads"
    (let [cram-header {:SQ
                       [{:SN "ref"}
                        {:SN "ref2"}]
                       :RG
                       [{:ID "rg001"}
                        {:ID "rg002"}]}
          tag-dict [[]
                    [{:tag :MD, :type \Z}
                     {:tag :NM, :type \c}]]
          ds-encoders (ds/build-data-series-encoders ds/default-data-series-encodings)
          tag-encoders (ds/build-tag-encoders
                        {:MD {\Z {:codec :byte-array-len
                                  :len-encoding {:codec :external, :content-id 5063770}
                                  :val-encoding {:codec :external, :content-id 5063770}}}
                         :NM {\c {:codec :byte-array-len
                                  :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
                                  :val-encoding {:codec :external, :content-id 5131619}}}})
          records [{:qname "q001", :flag 99, :rname "ref", :pos 1, :end 5, :mapq 0,
                    :cigar "5M", :rnext "=", :pnext 151, :tlen 150, :seq "AGAAT", :qual "HFHHH"
                    :options [{:RG {:type "Z", :value "rg001"}}
                              {:MD {:type "Z", :value "2C2"}}
                              {:NM {:type "c", :value 1}}]
                    ::record/tags-index 1}
                   {:qname "q002", :flag 99, :rname "ref", :pos 5, :end 7, :mapq 15,
                    :cigar "2S3M", :rnext "=", :pnext 15, :tlen 15, :seq "CCTGT", :qual "##AAC"
                    :options [{:RG {:type "Z", :value "rg001"}}
                              {:MD {:type "Z", :value "3"}}
                              {:NM {:type "c", :value 0}}]
                    ::record/tags-index 1}
                   {:qname "q003", :flag 177, :rname "ref", :pos 10, :end 14, :mapq 60,
                    :cigar "5M", :rnext "ref2", :pnext 100, :tlen 0, :seq "GATAA", :qual "CCCFF"
                    :options [{:RG {:type "Z", :value "rg002"}}
                              {:MD {:type "Z", :value "5"}}
                              {:NM {:type "c", :value 0}}]
                    ::record/tags-index 1}
                   {:qname "q004", :flag 147, :rname "ref", :pos 15, :end 19, :mapq 15,
                    :cigar "1M1I1M1D2M", :rnext "=", :pnext 5, :tlen -15, :seq "GAAAG", :qual "EBBFF"
                    :options [{:RG {:type "Z", :value "rg002"}}
                              {:MD {:type "Z", :value "3^T2"}}
                              {:NM {:type "c",  :value 2}}]
                    ::record/tags-index 1}
                   {:qname "q005", :flag 73, :rname "ref", :pos 20, :end 24, :mapq 0,
                    :cigar "5M", :rnext "*", :pnext 0, :tlen 0, :seq "CTGTG", :qual "AEEEE"
                    :options [], ::record/tags-index 0}]
          stats (record/encode-slice-records test-seq-resolver cram-header tag-dict subst-mat
                                             ds-encoders tag-encoders records)
          ds-res (walk/prewalk #(if (fn? %) (%) %) ds-encoders)
          tag-res (walk/prewalk #(if (fn? %) (%) %) tag-encoders)]
      (is (= {:ri 0, :start 1, :end 24, :nbases 25, :nrecords 5}
             (into {} stats)))

      (is (= 1 (count (get ds-res :BF))))
      (is (= 1 (get-in ds-res [:BF 0 :content-id])))
      (is (= [0x43 0x43 0x80 0x91 0x80 0x93 0x41]
             (map #(bit-and % 0xff) (get-in ds-res [:BF 0 :data]))))

      (is (= 1 (count (get ds-res :CF))))
      (is (= 2 (get-in ds-res [:CF 0 :content-id])))
      (is (= [3 3 3 3 3] (seq (get-in ds-res [:CF 0 :data]))))

      (is (= 1 (count (get ds-res :RI))))
      (is (= 3 (get-in ds-res [:RI 0 :content-id])))
      (is (= [0 0 0 0 0] (seq (get-in ds-res [:RI 0 :data]))))

      (is (= 1 (count (get ds-res :RL))))
      (is (= 4 (get-in ds-res [:RL 0 :content-id])))
      (is (= [5 5 5 5 5] (seq (get-in ds-res [:RL 0 :data]))))

      (is (= 1 (count (get ds-res :AP))))
      (is (= 5 (get-in ds-res [:AP 0 :content-id])))
      (is (= [1 5 10 15 20] (seq (get-in ds-res [:AP 0 :data]))))

      (is (= 1 (count (get ds-res :RG))))
      (is (= 6 (get-in ds-res [:RG 0 :content-id])))
      (is (= [0 0 1 1 0xff 0xff 0xff 0xff 0x0f]
             (map #(bit-and % 0xff) (get-in ds-res [:RG 0 :data]))))

      (is (= 1 (count (get ds-res :RN))))
      (is (= 7 (get-in ds-res [:RN 0 :content-id])))
      (is (= "q001\tq002\tq003\tq004\tq005\t" (String. ^bytes (get-in ds-res [:RN 0 :data]))))

      (is (= 1 (count (get ds-res :MF))))
      (is (= 8 (get-in ds-res [:MF 0 :content-id])))
      (is (= [1 1 1 0 2] (seq (get-in ds-res [:MF 0 :data]))))

      (is (= 1 (count (get ds-res :NS))))
      (is (= 9 (get-in ds-res [:NS 0 :content-id])))
      (is (= [0 0 1 0 0xff 0xff 0xff 0xff 0x0f]
             (map #(bit-and % 0xff) (get-in ds-res [:NS 0 :data]))))

      (is (= 1 (count (get ds-res :NP))))
      (is (= 10 (get-in ds-res [:NP 0 :content-id])))
      (is (= [0x80 0x97 0x0f 0x64 0x05 0x00]
             (map #(bit-and % 0xff) (get-in ds-res [:NP 0 :data]))))

      (is (= 1 (count (get ds-res :TS))))
      (is (= 11 (get-in ds-res [:TS 0 :content-id])))
      (is (= [0x80 0x96 0x0f 0x00 0xff 0xff 0xff 0xff 0x01 0x00]
             (map #(bit-and % 0xff) (get-in ds-res [:TS 0 :data]))))

      (is (= 1 (count (get ds-res :NF))))
      (is (= 12 (get-in ds-res [:NF 0 :content-id])))
      (is (zero? (count (get-in ds-res [:NF 0 :data]))))

      (is (= 1 (count (get ds-res :TL))))
      (is (= 13 (get-in ds-res [:TL 0 :content-id])))
      (is (= [1 1 1 1 0] (seq (get-in ds-res [:TL 0 :data]))))

      (is (= 1 (count (get ds-res :FN))))
      (is (= 14 (get-in ds-res [:FN 0 :content-id])))
      (is (= [1 1 0 2 0] (seq (get-in ds-res [:FN 0 :data]))))

      (is (= 1 (count (get ds-res :FC))))
      (is (= 15 (get-in ds-res [:FC 0 :content-id])))
      (is (= [(int \X) (int \S) (int \I) (int \D)]
             (seq (get-in ds-res [:FC 0 :data]))))

      (is (= 1 (count (get ds-res :FP))))
      (is (= 16 (get-in ds-res [:FP 0 :content-id])))
      (is (= [3 1 2 2] (seq (get-in ds-res [:FP 0 :data]))))

      (is (= 1 (count (get ds-res :DL))))
      (is (= 17 (get-in ds-res [:DL 0 :content-id])))
      (is (= [1] (seq (get-in ds-res [:DL 0 :data]))))

      (is (= 2 (count (get ds-res :BB))))
      (is (= 18 (get-in ds-res [:BB 0 :content-id])))
      (is (zero? (count (get-in ds-res [:BB 0 :data]))))
      (is (= 19 (get-in ds-res [:BB 1 :content-id])))
      (is (zero? (count (get-in ds-res [:BB 1 :data]))))

      (is (= 2 (count (get ds-res :QQ))))
      (is (= 20 (get-in ds-res [:QQ 0 :content-id])))
      (is (zero? (count (get-in ds-res [:QQ 0 :data]))))
      (is (= 21 (get-in ds-res [:QQ 1 :content-id])))
      (is (zero? (count (get-in ds-res [:QQ 1 :data]))))

      (is (= 1 (count (get ds-res :BS))))
      (is (= 22 (get-in ds-res [:BS 0 :content-id])))
      (is (= [0] (seq (get-in ds-res [:BS 0 :data]))))

      (is (= 2 (count (get ds-res :IN))))
      (is (= 23 (get-in ds-res [:IN 0 :content-id])))
      (is (= [1] (seq (get-in ds-res [:IN 0 :data]))))
      (is (= 24 (get-in ds-res [:IN 1 :content-id])))
      (is (= "A" (String. ^bytes (get-in ds-res [:IN 1 :data]))))

      (is (= 1 (count (get ds-res :RS))))
      (is (= 25 (get-in ds-res [:RS 0 :content-id])))
      (is (zero? (count (get-in ds-res [:RS 0 :data]))))

      (is (= 1 (count (get ds-res :PD))))
      (is (= 26 (get-in ds-res [:PD 0 :content-id])))
      (is (zero? (count (get-in ds-res [:PD 0 :data]))))

      (is (= 1 (count (get ds-res :HC))))
      (is (= 27 (get-in ds-res [:HC 0 :content-id])))
      (is (zero? (count (get-in ds-res [:HC 0 :data]))))

      (is (= 2 (count (get ds-res :SC))))
      (is (= 28 (get-in ds-res [:SC 0 :content-id])))
      (is (= [2] (seq (get-in ds-res [:SC 0 :data]))))
      (is (= 29 (get-in ds-res [:SC 1 :content-id])))
      (is (= "CC" (String. ^bytes (get-in ds-res [:SC 1 :data]))))

      (is (= 1 (count (get ds-res :MQ))))
      (is (= 30 (get-in ds-res [:MQ 0 :content-id])))
      (is (= [0 15 60 15 0] (seq (get-in ds-res [:MQ 0 :data]))))

      (is (= 1 (count (get ds-res :BA))))
      (is (= 31 (get-in ds-res [:BA 0 :content-id])))
      (is (zero? (count (get-in ds-res [:BA 0 :data]))))

      (is (= 1 (count (get ds-res :QS))))
      (is (= 32 (get-in ds-res [:QS 0 :content-id])))
      (is (= "HFHHH##AACCCCFFEBBFFAEEEE"
             (->> (get-in ds-res [:QS 0 :data])
                  (map #(+ (long %) 33))
                  byte-array
                  String.)))

      (is (= 2 (count (get-in tag-res [:MD \Z]))))
      (is (= 5063770
             (get-in tag-res [:MD \Z 0 :content-id])
             (get-in tag-res [:MD \Z 1 :content-id])))
      (is (= `[4 ~@(map int "2C2\0")
               2 ~@(map int "3\0")
               2 ~@(map int "5\0")
               5 ~@(map int "3^T2\0")]
             (seq (get-in tag-res [:MD \Z 0 :data]))
             (seq (get-in tag-res [:MD \Z 1 :data]))))

      (is (= 1 (count (get-in tag-res [:NM \c]))))
      (is (= 5131619 (get-in tag-res [:NM \c 0 :content-id])))
      (is (= [1 0 0 2] (seq (get-in tag-res [:NM \c 0 :data]))))))
  (testing "unmapped reads"
    (let [cram-header {:SQ
                       [{:SN "ref"}
                        {:SN "ref2"}]}
          tag-dict [[]]
          ds-encoders (ds/build-data-series-encoders ds/default-data-series-encodings)
          records [{:qname "q001", :flag 77, :rname "*", :pos 0, :end 0, :mapq 0,
                    :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "AATCC", :qual "CCFFF"
                    :options [], ::record/tags-index 0}
                   {:qname "q001", :flag 141, :rname "*", :pos 0, :end 0, :mapq 0,
                    :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "ATTGT", :qual "BDFAD"
                    :options [], ::record/tags-index 0}
                   {:qname "q002", :flag 77, :rname "*", :pos 0, :end 0, :mapq 0,
                    :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "TGGTA", :qual "ADDHF"
                    :options [], ::record/tags-index 0}
                   {:qname "q002", :flag 141, :rname "*", :pos 0, :end 0, :mapq 0,
                    :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "TCTTG", :qual "DDDFD"
                    :options [], ::record/tags-index 0}
                   {:qname "q003", :flag 77, :rname "*", :pos 0, :end 0, :mapq 0,
                    :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "GCACA", :qual "BCCFD"
                    :options [], ::record/tags-index 0}]
          stats (record/encode-slice-records test-seq-resolver cram-header tag-dict
                                             subst-mat ds-encoders {} records)
          ds-res (walk/prewalk #(if (fn? %) (%) %) ds-encoders)]
      (is (= {:ri -1, :start 0, :end 0, :nbases 25, :nrecords 5}
             (into {} stats)))

      (is (= 1 (count (get ds-res :BF))))
      (is (= 1 (get-in ds-res [:BF 0 :content-id])))
      (is (= [0x45 0x80 0x85 0x45 0x80 0x85 0x45]
             (map #(bit-and % 0xff) (get-in ds-res [:BF 0 :data]))))

      (is (= 1 (count (get ds-res :CF))))
      (is (= 2 (get-in ds-res [:CF 0 :content-id])))
      (is (= [3 3 3 3 3] (seq (get-in ds-res [:CF 0 :data]))))

      (is (= 1 (count (get ds-res :RI))))
      (is (= 3 (get-in ds-res [:RI 0 :content-id])))
      (is (= [0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f]
             (map #(bit-and % 0xff) (get-in ds-res [:RI 0 :data]))))

      (is (= 1 (count (get ds-res :RL))))
      (is (= 4 (get-in ds-res [:RL 0 :content-id])))
      (is (= [5 5 5 5 5] (seq (get-in ds-res [:RL 0 :data]))))

      (is (= 1 (count (get ds-res :AP))))
      (is (= 5 (get-in ds-res [:AP 0 :content-id])))
      (is (= [0 0 0 0 0] (seq (get-in ds-res [:AP 0 :data]))))

      (is (= 1 (count (get ds-res :RG))))
      (is (= 6 (get-in ds-res [:RG 0 :content-id])))
      (is (= [0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f]
             (map #(bit-and % 0xff) (get-in ds-res [:RG 0 :data]))))

      (is (= 1 (count (get ds-res :RN))))
      (is (= 7 (get-in ds-res [:RN 0 :content-id])))
      (is (= "q001\tq001\tq002\tq002\tq003\t" (String. ^bytes (get-in ds-res [:RN 0 :data]))))

      (is (= 1 (count (get ds-res :MF))))
      (is (= 8 (get-in ds-res [:MF 0 :content-id])))
      (is (= [2 2 2 2 2] (seq (get-in ds-res [:MF 0 :data]))))

      (is (= 1 (count (get ds-res :NS))))
      (is (= 9 (get-in ds-res [:NS 0 :content-id])))
      (is (= [0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f]
             (map #(bit-and % 0xff) (get-in ds-res [:NS 0 :data]))))

      (is (= 1 (count (get ds-res :NP))))
      (is (= 10 (get-in ds-res [:NP 0 :content-id])))
      (is (= [0 0 0 0 0] (seq (get-in ds-res [:NP 0 :data]))))

      (is (= 1 (count (get ds-res :TS))))
      (is (= 11 (get-in ds-res [:TS 0 :content-id])))
      (is (= [0 0 0 0 0] (seq (get-in ds-res [:TS 0 :data]))))

      (is (= 1 (count (get ds-res :NF))))
      (is (= 12 (get-in ds-res [:NF 0 :content-id])))
      (is (zero? (count (get-in ds-res [:NF 0 :data]))))

      (is (= 1 (count (get ds-res :TL))))
      (is (= 13 (get-in ds-res [:TL 0 :content-id])))
      (is (= [0 0 0 0 0] (seq (get-in ds-res [:TL 0 :data]))))

      (is (= 1 (count (get ds-res :FN))))
      (is (= 14 (get-in ds-res [:FN 0 :content-id])))
      (is (zero? (count (get-in ds-res [:FN 0 :data]))))

      (is (= 1 (count (get ds-res :FC))))
      (is (= 15 (get-in ds-res [:FC 0 :content-id])))
      (is (zero? (count (get-in ds-res [:FC 0 :data]))))

      (is (= 1 (count (get ds-res :FP))))
      (is (= 16 (get-in ds-res [:FP 0 :content-id])))
      (is (zero? (count (get-in ds-res [:FP 0 :data]))))

      (is (= 1 (count (get ds-res :DL))))
      (is (= 17 (get-in ds-res [:DL 0 :content-id])))
      (is (zero? (count (get-in ds-res [:DL 0 :data]))))

      (is (= 2 (count (get ds-res :BB))))
      (is (= 18 (get-in ds-res [:BB 0 :content-id])))
      (is (zero? (count (get-in ds-res [:BB 0 :data]))))
      (is (= 19 (get-in ds-res [:BB 1 :content-id])))
      (is (zero? (count (get-in ds-res [:BB 1 :data]))))

      (is (= 2 (count (get ds-res :QQ))))
      (is (= 20 (get-in ds-res [:QQ 0 :content-id])))
      (is (zero? (count (get-in ds-res [:QQ 0 :data]))))
      (is (= 21 (get-in ds-res [:QQ 1 :content-id])))
      (is (zero? (count (get-in ds-res [:QQ 1 :data]))))

      (is (= 1 (count (get ds-res :BS))))
      (is (= 22 (get-in ds-res [:BS 0 :content-id])))
      (is (zero? (count (get-in ds-res [:BS 0 :data]))))

      (is (= 2 (count (get ds-res :IN))))
      (is (= 23 (get-in ds-res [:IN 0 :content-id])))
      (is (zero? (count (get-in ds-res [:IN 0 :data]))))
      (is (= 24 (get-in ds-res [:IN 1 :content-id])))
      (is (zero? (count (get-in ds-res [:IN 1 :data]))))

      (is (= 1 (count (get ds-res :RS))))
      (is (= 25 (get-in ds-res [:RS 0 :content-id])))
      (is (zero? (count (get-in ds-res [:RS 0 :data]))))

      (is (= 1 (count (get ds-res :PD))))
      (is (= 26 (get-in ds-res [:PD 0 :content-id])))
      (is (zero? (count (get-in ds-res [:PD 0 :data]))))

      (is (= 1 (count (get ds-res :HC))))
      (is (= 27 (get-in ds-res [:HC 0 :content-id])))
      (is (zero? (count (get-in ds-res [:HC 0 :data]))))

      (is (= 2 (count (get ds-res :SC))))
      (is (= 28 (get-in ds-res [:SC 0 :content-id])))
      (is (zero? (count (get-in ds-res [:SC 0 :data]))))
      (is (= 29 (get-in ds-res [:SC 1 :content-id])))
      (is (zero? (count (get-in ds-res [:SC 1 :data]))))

      (is (= 1 (count (get ds-res :MQ))))
      (is (= 30 (get-in ds-res [:MQ 0 :content-id])))
      (is (zero? (count (get-in ds-res [:MQ 0 :data]))))

      (is (= 1 (count (get ds-res :BA))))
      (is (= 31 (get-in ds-res [:BA 0 :content-id])))
      (is (= "AATCCATTGTTGGTATCTTGGCACA"
             (String. ^bytes (get-in ds-res [:BA 0 :data]))))

      (is (= 1 (count (get ds-res :QS))))
      (is (= 32 (get-in ds-res [:QS 0 :content-id])))
      (is (= "CCFFFBDFADADDHFDDDFDBCCFD"
             (->> (get-in ds-res [:QS 0 :data])
                  (map #(+ (long %) 33))
                  byte-array
                  String.))))))

(ns cljam.io.cram.encode.record-test
  (:require [cljam.io.cram.encode.context :as context]
            [cljam.io.cram.encode.record :as record]
            [cljam.io.cram.encode.subst-matrix :as subst-mat]
            [cljam.io.cram.encode.tag-dict :as tag-dict]
            [cljam.io.cram.seq-resolver.protocol :as resolver]
            [cljam.io.sequence :as cseq]
            [cljam.test-common :as common]
            [clojure.test :refer [are deftest is testing]]
            [clojure.walk :as walk])
  (:import [java.util ArrayList]))

(def ^:private test-seq-resolver
  (let [seqs (with-open [r (cseq/reader common/test-fa-file)]
               (into {} (map (juxt :name :sequence)) (cseq/read-all-sequences r)))]
    (reify resolver/ISeqResolver
      (resolve-sequence [_ chr]
        (.getBytes ^bytes (get seqs chr)))
      (resolve-sequence [_ chr start end]
        (let [s (get seqs chr)]
          (.getBytes (subs s (dec (long start)) end)))))))

(defn- test-subst-mat-builder [m]
  (let [^objects arr (make-array Object 5 5)
        bs (map-indexed vector "ACGTN")]
    (doseq [[i r] bs
            [j a] bs
            :when (not= i j)]
      (aset ^objects (aget arr i) j
            (or (get-in m [r a]) (subst-mat/->MutableInt 0))))
    (subst-mat/->SubstMatrixBuilder arr)))

(deftest calculate-read-features&end
  (let [a->c (subst-mat/->MutableInt 0)
        c->t (subst-mat/->MutableInt 0)
        g->c (subst-mat/->MutableInt 0)
        subst-mat-builder (test-subst-mat-builder {\A {\C a->c}, \C {\T c->t}, \G {\C g->c}})]
    (are [?record ?expected]
         (= ?expected
            (walk/prewalk
             #(if (instance? (Class/forName "[B") %) (vec %) %)
             (#'record/calculate-read-features&end test-seq-resolver subst-mat-builder ?record)))
      {:rname "ref", :pos 2, :seq "GCATG", :qual "ABCDE", :cigar "5M"}
      [[]
       6]

      {:rname "ref", :pos 2, :seq "GCCTG", :qual "ABCDE", :cigar "5M"}
      [[{:code :subst, :pos 3, :subst a->c}]
       6]

      {:rname "ref", :pos 2, :seq "GCRTG", :qual "ABCDE", :cigar "5M"}
      [[{:code :read-base, :pos 3, :base (int \R), :qual (- (int \C) 33)}]
       6]

      {:rname "ref", :pos 2, :seq "GCCAA", :qual "ABCDE", :cigar "2M2I1M"}
      [[{:code :insertion, :pos 3, :bases (mapv int "CA")}]
       4]

      {:rname "ref", :pos 2, :seq "GTCAA", :qual "ABCDE", :cigar "2M2I1M"}
      [[{:code :subst, :pos 2, :subst c->t}
        {:code :insertion, :pos 3, :bases (mapv int "CA")}]
       4]

      {:rname "ref", :pos 2, :seq "GCGTT", :qual "ABCDE", :cigar "2M2D3M"}
      [[{:code :deletion, :pos 3, :len 2}]
       8]

      {:rname "ref", :pos 2, :seq "GCCTT", :qual "ABCDE", :cigar "2M2D3M"}
      [[{:code :deletion, :pos 3, :len 2}
        {:code :subst, :pos 3, :subst g->c}]
       8]

      {:rname "ref", :pos 2, :seq "GCATT", :qual "ABCDE", :cigar "3M2S"}
      [[{:code :softclip, :pos 4, :bases (mapv int "TT")}]
       4]

      {:rname "ref", :pos 2, :seq "TTGCA", :qual "ABCDE", :cigar "2S3M"}
      [[{:code :softclip, :pos 1, :bases (mapv int "TT")}]
       4])
    (is (= 1 @a->c))
    (is (= 1 @c->t))
    (is (= 1 @g->c))))

(defn- preprocess-slice-records [cram-header subst-mat-init records]
  (let [opts {:ds-compressor-overrides (constantly :raw)
              :tag-compressor-overrides (constantly (constantly (constantly {:external :raw})))}
        container-ctx (-> (context/make-container-context cram-header test-seq-resolver opts)
                          (assoc :subst-mat-builder (test-subst-mat-builder subst-mat-init)))
        stats (record/preprocess-slice-records container-ctx records)]
    (context/finalize-container-context container-ctx [stats])))

(deftest preprocess-slice-records-test
  (let [cram-header {:SQ [{:SN "ref"}]}
        c->a (subst-mat/->MutableInt 0)
        subst-mat-init {\C {\A c->a}}
        records (ArrayList.
                 [{:qname "q001", :flag 99, :rname "ref", :pos 1, :cigar "5M",
                   :rnext "=", :pnext 151, :tlen 150, :seq "AGAAT", :qual "HFHHH",
                   :options [{:RG {:type "Z", :value "rg001"}}
                             {:MD {:type "Z", :value "2C2"}}
                             {:NM {:type "c", :value 1}}]}
                  {:qname "q002", :flag 99, :rname "ref", :pos 5, :cigar "2S3M",
                   :rnext "=", :pnext 15, :tlen 15, :seq "CCTGT", :qual "##AAC"
                   :options [{:RG {:type "Z", :value "rg001"}}
                             {:MD {:type "Z", :value "3"}}
                             {:NM {:type "c", :value 0}}]}
                  {:qname "q003", :flag 177, :rname "ref", :pos 10, :cigar "5M",
                   :rnext "ref2", :pnext 100, :tlen 0, :seq "GATAA", :qual "CCCFF"
                   :options [{:RG {:type "Z", :value "rg002"}}
                             {:MD {:type "Z", :value "5"}}
                             {:NM {:type "c", :value 0}}]}
                  {:qname "q002", :flag 147, :rname "ref", :pos 15, :cigar "1M1I1M1D2M",
                   :rnext "=", :pnext 5, :tlen -15, :seq "GAAAG", :qual "EBBFF"
                   :options [{:RG {:type "Z", :value "rg002"}}
                             {:MD {:type "Z", :value "3^T2"}}
                             {:NM {:type "c",  :value 2}}]}
                  {:qname "q004", :flag 65, :rname "ref", :pos 20, :cigar "5M",
                   :rnext "=", :pnext 20, :tlen 0, :seq "CTGTG", :qual "DBBDD"
                   :options [{:RG {:type "Z", :value "rg001"}}
                             {:MD {:type "Z", :value "5"}}
                             {:NM {:type "c", :value 0}}]}
                  {:qname "q004", :flag 129, :rname "ref", :pos 20, :cigar "5M",
                   :rnext "=", :pnext 20, :tlen 0, :seq "CTGTG", :qual "DBBDD"
                   :options [{:RG {:type "Z", :value "rg001"}}
                             {:MD {:type "Z", :value "5"}}
                             {:NM {:type "c",  :value 0}}]}
                  {:qname "q005", :flag 77, :rname "*", :pos 0, :cigar "*",
                   :rnext "*", :pnext 0, :tlen 0, :seq "ATGCA", :qual "AEEEE"
                   :options []}
                  {:qname "q005", :flag 141, :rname "*", :pos 0, :cigar "*",
                   :rnext "*", :pnext 0, :tlen 0, :seq "*", :qual "*"
                   :options []}])
        container-ctx (preprocess-slice-records cram-header subst-mat-init records)]
    (is (= [{:qname "q001", :flag 99, :rname "ref", :pos 1, :cigar "5M",
             :rnext "=", :pnext 151, :tlen 150, :seq "AGAAT", :qual "HFHHH"
             :options [{:RG {:type "Z", :value "rg001"}}
                       {:MD {:type "Z", :value "2C2"}}
                       {:NM {:type "c", :value 1}}]
             ::record/flag 0x03, ::record/ref-index 0, ::record/end 5, ::record/tags-index 0,
             ::record/features [{:code :subst, :pos 3 :subst c->a}]}
            {:qname "q002", :flag 99, :rname "ref", :pos 5, :cigar "2S3M",
             :rnext "=", :pnext 15, :tlen 15, :seq "CCTGT", :qual "##AAC"
             :options [{:RG {:type "Z", :value "rg001"}}
                       {:MD {:type "Z", :value "3"}}
                       {:NM {:type "c", :value 0}}]
             ::record/flag 0x05, ::record/ref-index 0, ::record/end 7,
             ::record/tags-index 0, ::record/next-fragment 1,
             ::record/features [{:code :softclip, :pos 1, :bases [(int \C) (int \C)]}]}
            {:qname "q003", :flag 177, :rname "ref", :pos 10, :cigar "5M",
             :rnext "ref2", :pnext 100, :tlen 0,  :seq "GATAA", :qual "CCCFF"
             :options [{:RG {:type "Z", :value "rg002"}}
                       {:MD {:type "Z", :value "5"}}
                       {:NM {:type "c", :value 0}}]
             ::record/flag 0x03, ::record/ref-index 0, ::record/end 14, ::record/tags-index 0
             ::record/features []}
            {:qname "q002", :flag 147, :rname "ref", :pos 15, :cigar "1M1I1M1D2M",
             :rnext "=", :pnext 5, :tlen -15,  :seq "GAAAG", :qual "EBBFF"
             :options [{:RG {:type "Z", :value "rg002"}}
                       {:MD {:type "Z", :value "3^T2"}}
                       {:NM {:type "c",  :value 2}}]
             ::record/flag 0x01, ::record/ref-index 0, ::record/end 19, ::record/tags-index 0
             ::record/features [{:code :insertion, :pos 2, :bases [(int \A)]}
                                {:code :deletion, :pos 4, :len 1}]}
            {:qname "q004", :flag 65, :rname "ref", :pos 20, :cigar "5M",
             :rnext "=", :pnext 20, :tlen 0, :seq "CTGTG", :qual "DBBDD"
             :options [{:RG {:type "Z", :value "rg001"}}
                       {:MD {:type "Z", :value "5"}}
                       {:NM {:type "c", :value 0}}]
             ::record/flag 0x03, ::record/ref-index 0, ::record/end 24,
             ::record/tags-index 0, ::record/features []}
            {:qname "q004", :flag 129, :rname "ref", :pos 20, :cigar "5M",
             :rnext "=", :pnext 20, :tlen 0, :seq "CTGTG", :qual "DBBDD"
             :options [{:RG {:type "Z", :value "rg001"}}
                       {:MD {:type "Z", :value "5"}}
                       {:NM {:type "c",  :value 0}}]
             ::record/flag 0x03, ::record/ref-index 0, ::record/end 24,
             ::record/tags-index 0, ::record/features []}
            {:qname "q005", :flag 77, :rname "*", :pos 0, :cigar "*",
             :rnext "*", :pnext 0, :tlen 0, :seq "ATGCA", :qual "AEEEE", :options []
             ::record/flag 0x05, ::record/ref-index -1, ::record/end 0,
             ::record/tags-index 1, ::record/next-fragment 0,
             ::record/features []}
            {:qname "q005", :flag 141, :rname "*", :pos 0, :cigar "*",
             :rnext "*", :pnext 0, :tlen 0, :seq "*", :qual "*", :options []
             ::record/flag 0x09, ::record/ref-index -1, ::record/end 0, ::record/tags-index 1
             ::record/features []}]
           (walk/prewalk #(if (.isArray (class %)) (vec %) %)
                         (vec records))))
    (is (= 0 @c->a))
    (is (= [[{:tag :MD, :type \Z} {:tag :NM, :type \c}]
            []]
           (:tag-dict container-ctx)))
    (is (= {:MD {\Z {:codec :byte-array-len
                     :len-encoding {:codec :external
                                    :content-id (#'tag-dict/tag-id {:tag :MD, :type \Z})
                                    :compressor :raw}
                     :val-encoding {:codec :external
                                    :content-id (#'tag-dict/tag-id {:tag :MD, :type \Z})
                                    :compressor :raw}}}
            :NM {\c {:codec :byte-array-len
                     :len-encoding {:codec :huffman, :alphabet [1], :bit-len [0]}
                     :val-encoding {:codec :external
                                    :content-id (#'tag-dict/tag-id {:tag :NM, :type \c})
                                    :compressor :raw}}}}
           (:tag-encodings container-ctx)))))

(deftest encode-slice-records-test
  (testing "mapped reads"
    (let [cram-header {:HD {:SO "coordinate"}
                       :SQ
                       [{:SN "ref"}
                        {:SN "ref2"}]
                       :RG
                       [{:ID "rg001"}
                        {:ID "rg002"}]}
          c->a (subst-mat/->MutableInt 0)
          subst-mat-init {\C {\A c->a}}
          records (ArrayList.
                   [{:qname "q001", :flag 99, :rname "ref", :pos 1, :end 5, :mapq 0,
                     :cigar "5M", :rnext "=", :pnext 151, :tlen 150, :seq "AGAAT", :qual "HFHHH"
                     :options [{:RG {:type "Z", :value "rg001"}}
                               {:MD {:type "Z", :value "2C2"}}
                               {:NM {:type "c", :value 1}}]}
                    {:qname "q002", :flag 99, :rname "ref", :pos 5, :end 7, :mapq 15,
                     :cigar "2S3M", :rnext "=", :pnext 15, :tlen 15, :seq "CCTGT", :qual "##AAC"
                     :options [{:RG {:type "Z", :value "rg001"}}
                               {:MD {:type "Z", :value "3"}}
                               {:NM {:type "c", :value 0}}]}
                    {:qname "q003", :flag 177, :rname "ref", :pos 10, :end 14, :mapq 60,
                     :cigar "5M", :rnext "ref2", :pnext 100, :tlen 0, :seq "GATAA", :qual "CCCFF"
                     :options [{:RG {:type "Z", :value "rg002"}}
                               {:MD {:type "Z", :value "5"}}
                               {:NM {:type "c", :value 0}}]}
                    {:qname "q002", :flag 147, :rname "ref", :pos 15, :end 19, :mapq 15,
                     :cigar "1M1I1M1D2M", :rnext "=", :pnext 5, :tlen -15, :seq "GAAAG", :qual "EBBFF"
                     :options [{:RG {:type "Z", :value "rg002"}}
                               {:MD {:type "Z", :value "3^T2"}}
                               {:NM {:type "c", :value 2}}]}
                    {:qname "q004", :flag 73, :rname "ref", :pos 20, :end 24, :mapq 0,
                     :cigar "5M", :rnext "*", :pnext 0, :tlen 0, :seq "CTGTG", :qual "AEEEE"
                     :options []}])
          slice-ctx (-> (preprocess-slice-records cram-header subst-mat-init records)
                        (context/make-slice-context 0))
          _ (record/encode-slice-records slice-ctx records)
          ds-res (walk/prewalk #(if (fn? %) (%) %) (:ds-encoders slice-ctx))
          tag-res (walk/prewalk #(if (fn? %) (%) %) (:tag-encoders slice-ctx))]
      (is (= {:ri 0, :start 1, :end 24, :nbases 25, :nrecords 5}
             (into {} (:alignment-stats slice-ctx))))

      (is (= 1 (count (get ds-res :BF))))
      (is (= 2 (get-in ds-res [:BF 0 :content-id])))
      (is (= [0x43 0x43 0x80 0x91 0x80 0x93 0x41]
             (map #(bit-and % 0xff) (get-in ds-res [:BF 0 :data]))))

      (is (= 1 (count (get ds-res :CF))))
      (is (= 3 (get-in ds-res [:CF 0 :content-id])))
      (is (= [3 5 3 1 3] (seq (get-in ds-res [:CF 0 :data]))))

      (is (= 1 (count (get ds-res :RI))))
      (is (= 4 (get-in ds-res [:RI 0 :content-id])))
      (is (= [0 0 0 0 0] (seq (get-in ds-res [:RI 0 :data]))))

      (is (= 1 (count (get ds-res :RL))))
      (is (= 5 (get-in ds-res [:RL 0 :content-id])))
      (is (= [5 5 5 5 5] (seq (get-in ds-res [:RL 0 :data]))))

      (is (= 1 (count (get ds-res :AP))))
      (is (= 6 (get-in ds-res [:AP 0 :content-id])))
      (is (= [0 4 5 5 5] (seq (get-in ds-res [:AP 0 :data]))))

      (is (= 1 (count (get ds-res :RG))))
      (is (= 7 (get-in ds-res [:RG 0 :content-id])))
      (is (= [0 0 1 1 0xff 0xff 0xff 0xff 0x0f]
             (map #(bit-and % 0xff) (get-in ds-res [:RG 0 :data]))))

      (is (= 1 (count (get ds-res :RN))))
      (is (= 8 (get-in ds-res [:RN 0 :content-id])))
      (is (= "q001\tq002\tq003\tq002\tq004\t" (String. ^bytes (get-in ds-res [:RN 0 :data]))))

      (is (= 1 (count (get ds-res :MF))))
      (is (= 9 (get-in ds-res [:MF 0 :content-id])))
      (is (= [1 1 2] (seq (get-in ds-res [:MF 0 :data]))))

      (is (= 1 (count (get ds-res :NS))))
      (is (= 10 (get-in ds-res [:NS 0 :content-id])))
      (is (= [0 1 0xff 0xff 0xff 0xff 0x0f]
             (map #(bit-and % 0xff) (get-in ds-res [:NS 0 :data]))))

      (is (= 1 (count (get ds-res :NP))))
      (is (= 11 (get-in ds-res [:NP 0 :content-id])))
      (is (= [0x80 0x97 0x64 0x00]
             (map #(bit-and % 0xff) (get-in ds-res [:NP 0 :data]))))

      (is (= 1 (count (get ds-res :TS))))
      (is (= 12 (get-in ds-res [:TS 0 :content-id])))
      (is (= [0x80 0x96 0x00 0x00]
             (map #(bit-and % 0xff) (get-in ds-res [:TS 0 :data]))))

      (is (= 1 (count (get ds-res :NF))))
      (is (= 13 (get-in ds-res [:NF 0 :content-id])))
      (is (= [1] (seq (get-in ds-res [:NF 0 :data]))))

      (is (= 1 (count (get ds-res :TL))))
      (is (= 14 (get-in ds-res [:TL 0 :content-id])))
      (is (= [0 0 0 0 1] (seq (get-in ds-res [:TL 0 :data]))))

      (is (= 1 (count (get ds-res :FN))))
      (is (= 15 (get-in ds-res [:FN 0 :content-id])))
      (is (= [1 1 0 2 0] (seq (get-in ds-res [:FN 0 :data]))))

      (is (= 1 (count (get ds-res :FC))))
      (is (= 16 (get-in ds-res [:FC 0 :content-id])))
      (is (= [(int \X) (int \S) (int \I) (int \D)]
             (seq (get-in ds-res [:FC 0 :data]))))

      (is (= 1 (count (get ds-res :FP))))
      (is (= 17 (get-in ds-res [:FP 0 :content-id])))
      (is (= [3 1 2 2] (seq (get-in ds-res [:FP 0 :data]))))

      (is (= 1 (count (get ds-res :DL))))
      (is (= 18 (get-in ds-res [:DL 0 :content-id])))
      (is (= [1] (seq (get-in ds-res [:DL 0 :data]))))

      (is (= 2 (count (get ds-res :BB))))
      (is (= 19 (get-in ds-res [:BB 0 :content-id])))
      (is (zero? (count (get-in ds-res [:BB 0 :data]))))
      (is (= 20 (get-in ds-res [:BB 1 :content-id])))
      (is (zero? (count (get-in ds-res [:BB 1 :data]))))

      (is (= 2 (count (get ds-res :QQ))))
      (is (= 21 (get-in ds-res [:QQ 0 :content-id])))
      (is (zero? (count (get-in ds-res [:QQ 0 :data]))))
      (is (= 22 (get-in ds-res [:QQ 1 :content-id])))
      (is (zero? (count (get-in ds-res [:QQ 1 :data]))))

      (is (= 1 (count (get ds-res :BS))))
      (is (= 23 (get-in ds-res [:BS 0 :content-id])))
      (is (= [@c->a] (seq (get-in ds-res [:BS 0 :data]))))

      (is (= 2 (count (get ds-res :IN))))
      (is (= 24 (get-in ds-res [:IN 0 :content-id])))
      (is (= [1] (seq (get-in ds-res [:IN 0 :data]))))
      (is (= 25 (get-in ds-res [:IN 1 :content-id])))
      (is (= "A" (String. ^bytes (get-in ds-res [:IN 1 :data]))))

      (is (= 1 (count (get ds-res :RS))))
      (is (= 26 (get-in ds-res [:RS 0 :content-id])))
      (is (zero? (count (get-in ds-res [:RS 0 :data]))))

      (is (= 1 (count (get ds-res :PD))))
      (is (= 27 (get-in ds-res [:PD 0 :content-id])))
      (is (zero? (count (get-in ds-res [:PD 0 :data]))))

      (is (= 1 (count (get ds-res :HC))))
      (is (= 28 (get-in ds-res [:HC 0 :content-id])))
      (is (zero? (count (get-in ds-res [:HC 0 :data]))))

      (is (= 2 (count (get ds-res :SC))))
      (is (= 29 (get-in ds-res [:SC 0 :content-id])))
      (is (= [2] (seq (get-in ds-res [:SC 0 :data]))))
      (is (= 30 (get-in ds-res [:SC 1 :content-id])))
      (is (= "CC" (String. ^bytes (get-in ds-res [:SC 1 :data]))))

      (is (= 1 (count (get ds-res :MQ))))
      (is (= 31 (get-in ds-res [:MQ 0 :content-id])))
      (is (= [0 15 60 15 0] (seq (get-in ds-res [:MQ 0 :data]))))

      (is (= 1 (count (get ds-res :BA))))
      (is (= 32 (get-in ds-res [:BA 0 :content-id])))
      (is (zero? (count (get-in ds-res [:BA 0 :data]))))

      (is (= 1 (count (get ds-res :QS))))
      (is (= 33 (get-in ds-res [:QS 0 :content-id])))
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
          records (ArrayList.
                   [{:qname "q001", :flag 77, :rname "*", :pos 0, :end 0, :mapq 0,
                     :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "AATCC", :qual "CCFFF"
                     :options []}
                    {:qname "q001", :flag 141, :rname "*", :pos 0, :end 0, :mapq 0,
                     :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "ATTGT", :qual "BDFAD"
                     :options []}
                    {:qname "q002", :flag 77, :rname "*", :pos 0, :end 0, :mapq 0,
                     :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "TGGTA", :qual "ADDHF"
                     :options []}
                    {:qname "q002", :flag 141, :rname "*", :pos 0, :end 0, :mapq 0,
                     :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "TCTTG", :qual "DDDFD"
                     :options []}
                    {:qname "q003", :flag 77, :rname "*", :pos 0, :end 0, :mapq 0,
                     :cigar "*", :rnext "*", :pnext 0, :tlen 0, :seq "GCACA", :qual "BCCFD"
                     :options []}])
          slice-ctx (-> (preprocess-slice-records cram-header {} records)
                        (context/make-slice-context 0))
          _ (record/encode-slice-records slice-ctx records)
          ds-res (walk/prewalk #(if (fn? %) (%) %) (:ds-encoders slice-ctx))
          tag-res (walk/prewalk #(if (fn? %) (%) %) (:tag-encoders slice-ctx))]
      (is (= {:ri -1, :start 0, :end 0, :nbases 25, :nrecords 5}
             (into {} (:alignment-stats slice-ctx))))

      (is (= 1 (count (get ds-res :BF))))
      (is (= 2 (get-in ds-res [:BF 0 :content-id])))
      (is (= [0x45 0x80 0x85 0x45 0x80 0x85 0x45]
             (map #(bit-and % 0xff) (get-in ds-res [:BF 0 :data]))))

      (is (= 1 (count (get ds-res :CF))))
      (is (= 3 (get-in ds-res [:CF 0 :content-id])))
      (is (= [5 1 5 1 3] (seq (get-in ds-res [:CF 0 :data]))))

      (is (= 1 (count (get ds-res :RI))))
      (is (= 4 (get-in ds-res [:RI 0 :content-id])))
      (is (= [0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f]
             (map #(bit-and % 0xff) (get-in ds-res [:RI 0 :data]))))

      (is (= 1 (count (get ds-res :RL))))
      (is (= 5 (get-in ds-res [:RL 0 :content-id])))
      (is (= [5 5 5 5 5] (seq (get-in ds-res [:RL 0 :data]))))

      (is (= 1 (count (get ds-res :AP))))
      (is (= 6 (get-in ds-res [:AP 0 :content-id])))
      (is (= [0 0 0 0 0] (seq (get-in ds-res [:AP 0 :data]))))

      (is (= 1 (count (get ds-res :RG))))
      (is (= 7 (get-in ds-res [:RG 0 :content-id])))
      (is (= [0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f
              0xff 0xff 0xff 0xff 0x0f]
             (map #(bit-and % 0xff) (get-in ds-res [:RG 0 :data]))))

      (is (= 1 (count (get ds-res :RN))))
      (is (= 8 (get-in ds-res [:RN 0 :content-id])))
      (is (= "q001\tq001\tq002\tq002\tq003\t" (String. ^bytes (get-in ds-res [:RN 0 :data]))))

      (is (= 1 (count (get ds-res :MF))))
      (is (= 9 (get-in ds-res [:MF 0 :content-id])))
      (is (= [2] (seq (get-in ds-res [:MF 0 :data]))))

      (is (= 1 (count (get ds-res :NS))))
      (is (= 10 (get-in ds-res [:NS 0 :content-id])))
      (is (= [0xff 0xff 0xff 0xff 0x0f]
             (map #(bit-and % 0xff) (get-in ds-res [:NS 0 :data]))))

      (is (= 1 (count (get ds-res :NP))))
      (is (= 11 (get-in ds-res [:NP 0 :content-id])))
      (is (= [0] (seq (get-in ds-res [:NP 0 :data]))))

      (is (= 1 (count (get ds-res :TS))))
      (is (= 12 (get-in ds-res [:TS 0 :content-id])))
      (is (= [0] (seq (get-in ds-res [:TS 0 :data]))))

      (is (= 1 (count (get ds-res :NF))))
      (is (= 13 (get-in ds-res [:NF 0 :content-id])))
      (is (= [0 0] (seq (get-in ds-res [:NF 0 :data]))))

      (is (= 1 (count (get ds-res :TL))))
      (is (= 14 (get-in ds-res [:TL 0 :content-id])))
      (is (= [0 0 0 0 0] (seq (get-in ds-res [:TL 0 :data]))))

      (is (= 1 (count (get ds-res :FN))))
      (is (= 15 (get-in ds-res [:FN 0 :content-id])))
      (is (zero? (count (get-in ds-res [:FN 0 :data]))))

      (is (= 1 (count (get ds-res :FC))))
      (is (= 16 (get-in ds-res [:FC 0 :content-id])))
      (is (zero? (count (get-in ds-res [:FC 0 :data]))))

      (is (= 1 (count (get ds-res :FP))))
      (is (= 17 (get-in ds-res [:FP 0 :content-id])))
      (is (zero? (count (get-in ds-res [:FP 0 :data]))))

      (is (= 1 (count (get ds-res :DL))))
      (is (= 18 (get-in ds-res [:DL 0 :content-id])))
      (is (zero? (count (get-in ds-res [:DL 0 :data]))))

      (is (= 2 (count (get ds-res :BB))))
      (is (= 19 (get-in ds-res [:BB 0 :content-id])))
      (is (zero? (count (get-in ds-res [:BB 0 :data]))))
      (is (= 20 (get-in ds-res [:BB 1 :content-id])))
      (is (zero? (count (get-in ds-res [:BB 1 :data]))))

      (is (= 2 (count (get ds-res :QQ))))
      (is (= 21 (get-in ds-res [:QQ 0 :content-id])))
      (is (zero? (count (get-in ds-res [:QQ 0 :data]))))
      (is (= 22 (get-in ds-res [:QQ 1 :content-id])))
      (is (zero? (count (get-in ds-res [:QQ 1 :data]))))

      (is (= 1 (count (get ds-res :BS))))
      (is (= 23 (get-in ds-res [:BS 0 :content-id])))
      (is (zero? (count (get-in ds-res [:BS 0 :data]))))

      (is (= 2 (count (get ds-res :IN))))
      (is (= 24 (get-in ds-res [:IN 0 :content-id])))
      (is (zero? (count (get-in ds-res [:IN 0 :data]))))
      (is (= 25 (get-in ds-res [:IN 1 :content-id])))
      (is (zero? (count (get-in ds-res [:IN 1 :data]))))

      (is (= 1 (count (get ds-res :RS))))
      (is (= 26 (get-in ds-res [:RS 0 :content-id])))
      (is (zero? (count (get-in ds-res [:RS 0 :data]))))

      (is (= 1 (count (get ds-res :PD))))
      (is (= 27 (get-in ds-res [:PD 0 :content-id])))
      (is (zero? (count (get-in ds-res [:PD 0 :data]))))

      (is (= 1 (count (get ds-res :HC))))
      (is (= 28 (get-in ds-res [:HC 0 :content-id])))
      (is (zero? (count (get-in ds-res [:HC 0 :data]))))

      (is (= 2 (count (get ds-res :SC))))
      (is (= 29 (get-in ds-res [:SC 0 :content-id])))
      (is (zero? (count (get-in ds-res [:SC 0 :data]))))
      (is (= 30 (get-in ds-res [:SC 1 :content-id])))
      (is (zero? (count (get-in ds-res [:SC 1 :data]))))

      (is (= 1 (count (get ds-res :MQ))))
      (is (= 31 (get-in ds-res [:MQ 0 :content-id])))
      (is (zero? (count (get-in ds-res [:MQ 0 :data]))))

      (is (= 1 (count (get ds-res :BA))))
      (is (= 32 (get-in ds-res [:BA 0 :content-id])))
      (is (= "AATCCATTGTTGGTATCTTGGCACA"
             (String. ^bytes (get-in ds-res [:BA 0 :data]))))

      (is (= 1 (count (get ds-res :QS))))
      (is (= 33 (get-in ds-res [:QS 0 :content-id])))
      (is (= "CCFFFBDFADADDHFDDDFDBCCFD"
             (->> (get-in ds-res [:QS 0 :data])
                  (map #(+ (long %) 33))
                  byte-array
                  String.)))

      (is (= {} tag-res)))))
